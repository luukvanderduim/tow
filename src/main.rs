/*   Copyright 2019 Luuk van der Duim

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to use,
copy, modify, merge, ish, distribute, sublicense, and/or sell copies of the
Software, and to permit persons to whom the Software is furnished to do so, subject
to the following conditions:

The above copyright notice and this permission notice shall be included in all copies
or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE. */

/*
tow: an ergonomy helper for desktop zoom users.
tow has the zoom area be 'towed by the keyboard caret'.
It is made with the Xfce4 desktop in mind,
however it might work with other desktop environments aswell.
*/
// #![warn(clippy::all)]
#![feature(duration_as_u128)]
#![feature(extern_types)]
#![allow(unused_imports)]
#![allow(non_camel_case_types)]
#![feature(exclusive_range_pattern)]

use gtypes::gpointer;
use daemonize::Daemonize;
use libc;
use glib_sys::{GDestroyNotify, GError, GHashTable, GPtrArray};
use gobject_sys::*;
use gtypes::primitive::{gboolean, gchar, gint, guint};
use kahan::{KahanSum, KahanSummator};
use libxdo::XDo;
use std::f64::consts::E;
use std::ffi::CString;
use std::fs::File;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use xcb;
use xcb::base::Connection;

mod atspi_ffi;
use atspi_ffi::*;

const SLIDE_DUR: Duration = Duration::from_micros(1_000_000);
const FRAME_DUR: Duration = Duration::from_micros(1_000_000 / 30);

#[derive(Clone, Copy, Debug)]
struct CaretTowState {
    counter: i32,
    timestamp: Option<Instant>,
    pointer_at_begin: (i32, i32),
    glyph_coords_end: (i32, i32),
    glyph_coords_begin: (i32, i32),
    behavior: Behavior,
}
impl CaretTowState {
    fn reset(&mut self) {
        self.counter = 0;
        self.pointer_at_begin = (0, 0);
        self.glyph_coords_end = (0, 0);
        self.glyph_coords_begin = (0, 0);
    }
    fn advance(&mut self, l: (i32, i32)) {
        self.counter += 1;
        self.glyph_coords_begin = l;
    }
}

type Duet<'a> = (&'a mut CaretTowState, *mut libxdo_sys::Struct_xdo);

#[derive(Clone, Copy, Debug)]
enum Behavior {
    Periodically { dur: Duration },
    By_Characters { nc: i32 },
}

/* enum move_style {
    sigmoid,
    warp,
    typewriter,
    parabolic,
} */

// static ms: move_style = sigmoid;

// 'Sigmoid' (S-shaped curve). Tow uses tanh in 0..2 range to model 'smooth' pointer animation.

fn get_sigmoid() -> Vec<f64> {
    let xd: f64 = 2.0 / ((SLIDE_DUR.as_micros() / FRAME_DUR.as_micros()) / 2) as f64;
    let sigmoid: Vec<f64> = (0..2_000_000)
        .step_by((xd * f64::from(1_000_000)) as usize)
        .map(|x| f64::from(x) / f64::from(1_000_000))
        .map(|x| (E.powf(x) - E.powf(-x)) / (E.powf(x) + E.powf(-x)))
        .collect();

    let ksum: KahanSum<f64> = sigmoid.clone().into_iter().kahan_sum();

    sigmoid
        .clone()
        .into_iter()
        .map(|x| x / ksum.sum())
        .collect()
} //FIXME SIGMOID DOES NOT ADD UP???

#[cfg(test)]
#[test]
fn test_get_sigmoid() {
    assert_eq!(get_sigmoid().into_iter().sum(), 1.0f64);
}


fn get_move_queue() -> Vec<f64> {
    let mut halfqueue = get_sigmoid().clone();
    let mut queue: Vec<f64> = get_sigmoid().clone();

    // Add halfqueue to queue in reversed order.
    // queue -> slope up /\ slope down
    while !halfqueue.is_empty() {
        if let Some(v) = halfqueue.pop() {
            queue.push(v);
        } else {
            eprintln!("Wonderous machine!");
        }
    }
    queue
}

fn do_tow(dx: i32, dy: i32) {
    let xdo = libxdo::XDo::new(None).expect("Failed to obtain XDo handle.");
    let mq = get_move_queue();
    do_move(xdo, mq, dx, dy, 0.0);

    // xdo.move_mouse_relative(dx, dy).expect("Failed to move!");
}
fn do_move(xdo: XDo, mut qu: Vec<f64>, mut dx: i32, mut dy: i32, cx: f64) {
    if !qu.is_empty() {
        std::thread::sleep(FRAME_DUR);
        let xmove_amount: f64;
        let y_as_xfraction: f64 = dy as f64 / dx as f64;

        if let Some(value) = qu.first() {
            xmove_amount = value * dx as f64 + cx;
            qu.remove(0);
        } else {
            eprintln!("Wonderous machine!! do_tow failed in calculating move");
            return;
        }

        if xmove_amount < 1.0 {
            do_move(xdo, qu, dx, dy, xmove_amount);
        } else {
            let amount_y: i32 = (xmove_amount * y_as_xfraction).trunc() as i32;
            xdo.move_mouse_relative(xmove_amount.trunc() as i32, amount_y)
                .expect("Failed to move!");
            dx -= xmove_amount.trunc() as i32;
            dy -= amount_y;
            do_move(xdo, qu, dx, dy, xmove_amount.trunc());
        }
    }
}

fn get_pointer_coordinates() -> (i32, i32) {
    let (conn, screen_num) = xcb::Connection::connect(None).expect("Failed xcb connection.");
    let setup = conn.get_setup();
    let screen = setup.roots().nth(screen_num as usize).unwrap();
    let root_id = screen.root();

    let pointercookie = xcb::xproto::query_pointer(&conn, root_id);

    match pointercookie.get_reply() {
        Ok(r) => {
            return (r.root_x() as i32, r.root_y() as i32);
        }
        Err(_) => {
            panic!("could not get coordinates of pointer");
        }
    };
}


extern "C" fn destroy_evgarbage(data: gpointer) {
    unsafe { libc::free(data) };
}

extern "C" fn on_caret_move(event: *mut AtspiEvent, vpdata: *mut ::std::os::raw::c_void) {
    use std::ptr::null_mut;

    // We can convert the pointer-type of vpdate
    // but cannot dereference
    // due to violation of the borrowed nature of its contents.
    // (ihope-iiuc)
    // "Can't eat a lent cake"-error
    // We can however make the raw into a normal &mut
    let pdata = vpdata as *mut Duet;
    let data = unsafe { &mut *pdata };
    let text_iface = unsafe { atspi_accessible_get_text_iface((*event).source) };
    let mut caret_offset = unsafe { atspi_text_get_caret_offset(text_iface, null_mut()) };

    // 'Detail1' might be the mask of event(s) we registered for (?)
    // Problem is that it is undocumented.
    if unsafe { (*event).detail1 == 0 } {
        return;
    }

    // Because we cannot ask for the co-ordinates of the caret directly,
    // we ask for the bounding box of the glyph at the position one before the carets.
    // (often there will not be a glyph at the carets position and we'll have better
    // chances at the preceding position) */
    let glyph_extents = unsafe {
        atspi_text_get_character_extents(
            text_iface,
            caret_offset - 1,
            ATSPI_COORD_TYPE_SCREEN,
            null_mut(), // GError
        )
    };

    // Often atspi_text_get_character_extents() seems to return twice:
    // Once with a valid value and once, supposedly at the origin of the screen
    // Some applications suffer from this, others dont. (Bluefish doesnt, xed does)
    // Until the cause is found and fixed, filter these.

    let caret_coords_now: (i32, i32) = unsafe { ((*glyph_extents).x, (*glyph_extents).y) };
    if caret_coords_now == (0, 0) {
        return;
    }

    // 'Move per X events'is fine as strategy until:
    // - somewhere between 0..10 the mouse pointer is moved
    // and with it our view port.

    // Rust will assure we'll never consume the borrowed cts: cts_curr
    // We end up with a '&mut &mut', Can we do a single &mut?
    let (cts_curr, pxdo_sys) = data;
    let pointer_coordinates = get_pointer_coordinates();



    let mut periodically = | dur, cts_curr: &mut &mut CaretTowState | {
        match (**cts_curr).timestamp {
            Some(timestamp) if timestamp.elapsed() < dur  => {    return;  },
            Some(timestamp) if timestamp.elapsed() >= dur  => {
                        (**cts_curr).glyph_coords_end = caret_coords_now; // We could use caret now directly
                        let dx: i32 = (**cts_curr).glyph_coords_begin.0 - cts_curr.glyph_coords_end.0;
                        let dy: i32 = (**cts_curr).glyph_coords_begin.1 - cts_curr.glyph_coords_end.1;
                        cts_curr.reset();
                        (**cts_curr).timestamp = None;
                        do_tow(dx, dy);
                    },
        None =>  {
            (**cts_curr).timestamp = Some(Instant::now());
            (**cts_curr).pointer_at_begin = pointer_coordinates;
            (**cts_curr).glyph_coords_begin = caret_coords_now;
            },
        Some(_) => { panic!("Wonderous machine: unreachable state timestamp value");  },
        };
    };

    let mut every_so_much_characters = | nc, cts_curr: &mut &mut CaretTowState | {
        if (**cts_curr).counter == 0 {
                    (**cts_curr).counter += 1;
                    (**cts_curr).pointer_at_begin = pointer_coordinates;
                    (**cts_curr).glyph_coords_end = caret_coords_now;
                    (**cts_curr).glyph_coords_begin = caret_coords_now;
            }
        else if ((**cts_curr).counter > 0 &&  (**cts_curr).counter<=nc)   {
                    if (**cts_curr).pointer_at_begin != pointer_coordinates {
                        (**cts_curr).reset();
                    } else {
                        (**cts_curr).advance(caret_coords_now);
                    }
            }
        else    {
                    if (**cts_curr).pointer_at_begin != pointer_coordinates {
                        (**cts_curr).reset();
                    } else {
                        let dx: i32 = (**cts_curr).glyph_coords_begin.0 - cts_curr.glyph_coords_end.0;
                        let dy: i32 = (**cts_curr).glyph_coords_begin.1 - cts_curr.glyph_coords_end.1;
                        (**cts_curr).reset();
                        do_tow(dx, dy);
                    }
        }

    };

    match (**cts_curr).behavior {
            Behavior::Periodically { dur } => periodically( dur.clone(), cts_curr ),
            Behavior::By_Characters { nc } => every_so_much_characters( nc.clone(), cts_curr ),
    };

}

fn spookify_tow() {
    let stdout = File::create("/tmp/tow-daemon.out").unwrap();
    let stderr = File::create("/tmp/tow-daemon.err").unwrap();

    let daemonize = Daemonize::new()
        .pid_file("/tmp/tow.pid") // Every method except `new` and `start`
        .working_directory("/tmp") // for default behaviour.
        .umask(0o777) // Set umask, `0o027` by default.
        .stdout(stdout) // Redirect stdout to `/tmp/daemon.out`.
        .stderr(stderr) // Redirect stderr to `/tmp/daemon.err`.
        .privileged_action(|| "Executed before drop privileges");

    match daemonize.start() {
        Ok(_) => println!("Success, daemonized"),
        Err(e) => eprintln!("Error, {}", e),
    }
}

fn main() {
   // spookify_tow();
    let manner = Behavior::Periodically { dur: Duration::from_micros(3_000_000) };
 // let manner = Behavior::By_Characters { nc: 10 };

    // Shared state between CBs
    let mut cts: CaretTowState = CaretTowState {
        counter: 0,
        timestamp: None,
        pointer_at_begin: (0, 0),
        glyph_coords_end: (0, 0),
        glyph_coords_begin: (0, 0),
        behavior: manner,
    };

     let pxdo_sys = unsafe { libxdo_sys::xdo_new(std::ptr::null()) };

    // In data, the state is borrowed (to avoid consumption )
    // This means the CB must return before the new event happens for
    // the state to be a coherent reflection of affairs.
    // Iow: we might miss events (characters) if we do not return in time.

    let data: Duet = (&mut cts, pxdo_sys);
    let vpdata = &data as *const Duet as *mut libc::c_void;
    let evfn: AtspiEventListenerCB = Some(on_caret_move);
    let evdestroygarb: GDestroyNotify = Some(destroy_evgarbage);

    // AT-SPI event listener VANILLA
    let listener = unsafe { atspi_event_listener_new(evfn, vpdata, evdestroygarb) };

    // AT-SPI init
    if unsafe { atspi_init() } != 0 {
        panic!("Could not initialise AT-SPI.");
    }

    let evtype = CString::new("object:text-caret-moved")
        .expect("CString::new failed")
        .into_raw() as *const i8;

    let err: *mut *mut GError = std::ptr::null_mut(); //Must do better

    unsafe {
        atspi_event_listener_register(listener, evtype, err);
    }
    unsafe {
        atspi_event_main();
    }

    if unsafe { atspi_exit() } != 0 {
        eprintln!("AT-SPI exit failed.");
    }

    unsafe {
        gobject_sys::g_object_unref(err as *mut gobject_sys::GObject);
        gobject_sys::g_object_unref(evtype as *mut gobject_sys::GObject);
        gobject_sys::g_object_unref(listener as *mut gobject_sys::GObject);
        gobject_sys::g_object_unref(vpdata as *mut gobject_sys::GObject);
    }
}
