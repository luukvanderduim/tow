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
// #![feature(duration_as_u128)]
#![feature(extern_types, exclusive_range_pattern)]
#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use daemonize::Daemonize;
use glib_sys::{GDestroyNotify, GError, GHashTable, GPtrArray};
use gobject_sys::*;
use gtypes::gpointer;
use gtypes::primitive::{gboolean, gchar, gint, guint};
use libc;
use std::f64::consts::E;
use std::ffi::CString;
use std::fs::File;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use xcb;
use xcb::base::Connection;

mod atspi_ffi;
use atspi_ffi::*;

const SLIDE_DUR: Duration = Duration::from_millis(1000);
const FRAME_DUR: Duration = Duration::from_millis(1000 / 30);
const XCB_NONE: u32 = 0;


#[derive(Clone, Copy, Debug)]
struct CaretTowState {
    counter: i32,
    timestamp: Option<Instant>,
    pointer_at_begin: (i32, i32),
    glyph_coords_end: (i32, i32),
    glyph_coords_begin: (i32, i32),
    evsname: Option<*const ::std::os::raw::c_char>,
    detail: gint,
    behavior: Behavior,
}
impl CaretTowState {
    fn reset(&mut self) {
        self.counter = 0;
        self.detail = 0;
        self.timestamp = None;
        self.evsname = None;
        self.pointer_at_begin = (0, 0);
        self.glyph_coords_end = (0, 0);
        self.glyph_coords_begin = (0, 0);
    }
    fn advance(&mut self, l: (i32, i32)) {
        self.counter += 1;
        self.glyph_coords_begin = l;
    }
}

#[derive(Clone, Copy, Debug)]
enum Behavior {
    Periodically { dur: Duration },
    By_Characters { nc: i32 },
    Typewriter,
}

//  .map(|x| (E.powf(x) - E.powf(-x)) / (E.powf(x) + E.powf(-x))) // tanh
// get_sigmoid makes us a S-shaped graph data
// a fluent slope within certain bounds
fn get_sigmoid() -> Vec<f64> {
    let frpslide = ((SLIDE_DUR.as_secs() * 1000_u64 + SLIDE_DUR.subsec_millis() as u64)
        / (FRAME_DUR.as_secs() * 1000_u64 + FRAME_DUR.subsec_millis() as u64))
        as f64;
    let dx: f64 = 1.0 / (frpslide * 0.5);
    let sigmoid: Vec<f64> = (0..1000)
        .step_by((dx * f64::from(1000)) as usize)
        .map(|x| f64::from(x) / f64::from(1000))
        .map(|x| f64::from(1) / f64::from(1.0_f64 + E.powf(12.0_f64 * x - 6.0_f64)))
        .collect();

    sigmoid
}


//
// get_move_shape yields a _/\_ shaped curve
// It consists of two sigmoids of which one is reversed.
fn get_move_shape() -> Vec<f64> {
    let mut right_half = get_sigmoid();
    let mut cl = get_sigmoid();
    let mut left_half: Vec<f64> = Vec::new();

    while !cl.is_empty() {
        if let Some(element) = cl.pop() {
            left_half.push(element);
        }
    }
    left_half.append(&mut right_half);
    left_half
}
#[cfg(test)]
#[test]
fn test_get_move_shape() {
    assert_eq!(get_move_shape().len(), 2 * get_sigmoid().len());
    assert_eq!(get_move_shape().first(), get_move_shape().last());
}

fn do_tow(mut dx: i32, mut dy: i32) {
    let mut values = get_move_shape();
    let nframes = values.len();
    let mut cx: f64 = 0.0;
    let mut xmove_amount: f64 = 0.0;
    let mut ymove_amount: f64 = 0.0;
    let mut y_in_x: f64 = dy as f64 / dx as f64;

    while !values.is_empty() {
        if let Some(value) = values.first() {
            xmove_amount = (value * dx as f64 + cx as f64) / (nframes as f64 / 2.0) as f64;
            values.remove(0);
        } else {
            panic!("First value in values appears to not be Some().");
        }

        if xmove_amount.abs() < 1.0 {
            cx = xmove_amount;
            continue; // next iteration of while
        } else if xmove_amount.abs() >= 1.0 {
            ymove_amount = xmove_amount * y_in_x;
            warp(xmove_amount.trunc() as i32, ymove_amount.trunc() as i32);
            cx = xmove_amount.fract();
            std::thread::sleep(FRAME_DUR);
        } else {
            panic!("Wonderous machine! xmove_amount abnormal.");
        }
    }
}

fn warp(x: i32, y: i32) {
   let (conn, screen_num) = xcb::Connection::connect(None).expect("Failed xcb connection.");

   xcb::warp_pointer(&conn, XCB_NONE, XCB_NONE, 0 as i16, 0 as i16,  0 as u16, 0 as u16, x as i16, y as i16).request_check().unwrap();
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
    let pdata = vpdata as *mut CaretTowState;
    let data = unsafe { &mut *pdata };
    let text_iface = unsafe { atspi_accessible_get_text_iface((*event).source) };
    let caret_offset = unsafe { atspi_text_get_caret_offset(text_iface, null_mut()) };
    let event_source_name: *const ::std::os::raw::c_char =
        unsafe { (*(*event).source).name } as *const ::std::os::raw::c_char;

    // 'Detail1' might be the bew index of caret-moved, though due to async implementation in the library,
    // may well not be consistent with current state.
    // https://accessibility.linuxfoundation.org/a11yspecs/atspi/adoc/atspi-events.html

    if unsafe { !(*event).detail1 >= 1 } {
        return;
    } //  ( Be some index, otherwise bail )

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
    let mut cts_curr = data;
    let pointer_coordinates = get_pointer_coordinates();

    let periodically = |dur, cts_curr: &mut &mut CaretTowState| {
        match cts_curr.timestamp {
            Some(timestamp) if timestamp.elapsed() < dur => {
                if cts_curr.pointer_at_begin != pointer_coordinates {
                    cts_curr.reset();
                }
            }
            Some(timestamp) if timestamp.elapsed() >= dur => {
                let dx: i32 = caret_coords_now.0 - cts_curr.glyph_coords_begin.0;
                let dy: i32 = caret_coords_now.1 - cts_curr.glyph_coords_begin.1;
                cts_curr.reset();
                do_tow(dx, dy);
                return;
            }
            None => {
                cts_curr.timestamp = Some(Instant::now());
                cts_curr.pointer_at_begin = pointer_coordinates;
                cts_curr.glyph_coords_begin = caret_coords_now;
            }
            Some(_) => {
                panic!("Wonderous machine: unreachable state timestamp value");
            }
        };
    };

    let every_so_much_characters = |nc, cts_curr: &mut &mut CaretTowState| {
        if cts_curr.counter == 0 {
            cts_curr.counter += 1;
            cts_curr.pointer_at_begin = pointer_coordinates;
            cts_curr.glyph_coords_end = caret_coords_now;
            cts_curr.glyph_coords_begin = caret_coords_now;
            return;
        } else if cts_curr.counter > 0 && (**cts_curr).counter <= nc {
            if cts_curr.pointer_at_begin != pointer_coordinates {
                cts_curr.reset();
                return;
            } else {
                cts_curr.advance(caret_coords_now);
                return;
            }
        } else {
            if cts_curr.pointer_at_begin != pointer_coordinates {
                cts_curr.reset();
                return;
            } else {
                let dx: i32 = caret_coords_now.0 - cts_curr.glyph_coords_begin.0;
                let dy: i32 = caret_coords_now.1 - cts_curr.glyph_coords_begin.1;
                cts_curr.reset();
                do_tow(dx, dy);
                return;
            }
        }
    };

    let every_other_character = |cts_curr: &mut &mut CaretTowState| {
        if cts_curr.counter == 0 {
            cts_curr.counter += 1;
            cts_curr.glyph_coords_begin = caret_coords_now;
            cts_curr.pointer_at_begin = pointer_coordinates;
        } else {
            let x = caret_coords_now.0 - cts_curr.glyph_coords_begin.0;
            let y = caret_coords_now.1 - cts_curr.glyph_coords_begin.1;
            warp(x, y);
            cts_curr.reset();
            return;
        }
    };

    match cts_curr.behavior {
        Behavior::Periodically { dur } => periodically(dur, &mut cts_curr),
        Behavior::By_Characters { nc } => every_so_much_characters(nc, &mut cts_curr),
        Behavior::Typewriter => every_other_character(&mut cts_curr),
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
    /*  let manner = Behavior::Periodically {
            dur: Duration::from_millis(2500),
    }; */
    let manner = Behavior::Typewriter;
    // let manner = Behavior::By_Characters { nc: 15 };

    // Shared state between CBs
    let mut cts: CaretTowState = CaretTowState {
        counter: 0,
        timestamp: None,
        evsname: None,
        detail: 0,
        pointer_at_begin: (0, 0),
        glyph_coords_end: (0, 0),
        glyph_coords_begin: (0, 0),
        behavior: manner,
    };

    // In data, the state is borrowed (to avoid consumption )
    // This means the CB must return before the new event happens for
    // the state to be a coherent reflection of affairs.
    // Iow: we might miss events (characters) if we do not return in time.

    let data = &mut cts;
    let vpdata = data as *mut CaretTowState as *mut libc::c_void;
    let evfn: AtspiEventListenerCB = Some(on_caret_move);
    let evdestroygarb: GDestroyNotify = Some(destroy_evgarbage);

    // AT-SPI event listener
    let listener = unsafe { atspi_event_listener_new(evfn, vpdata, evdestroygarb) };

    // AT-SPI init
    if unsafe { atspi_init() } != 0 {
        panic!("Could not initialise AT-SPI.");
    }

    // "object:event-from-input"

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
