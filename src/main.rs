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

#![feature(duration_as_u128)]
#![feature(extern_types)]
#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use daemonize::Daemonize;
use glib;
use glib_sys::{GDestroyNotify, GError, GHashTable, GPtrArray};
use gobject_sys::*;
use gtypes::primitive::{gboolean, gchar, gint, guint};
use kahan::{KahanSum, KahanSummator};
use libc;
use libxdo::XDo;
use libxdo_sys::*;
use std::cell::Cell;
use std::f64::consts::E;
use std::ffi::*;
use std::ffi::{CStr, CString};
use std::fs::File;
use std::os::raw::c_char;
use std::ptr::*;
use std::thread;
use std::time::{Duration, Instant};

const SLIDE_DUR: Duration = Duration::from_micros(1_000_000);
const FRAME_DUR: Duration = Duration::from_micros(1_000_000 / 30);

#[derive(Clone, Copy, Debug)]
struct CaretTowState {
    counter: i32,
    first_pos: (i32, i32),
    last_pos: (i32, i32),
}

static mut caret_tow_state: Cell<CaretTowState> = Cell::new(CaretTowState {
    counter: 0,
    first_pos: (0, 0),
    last_pos: (0, 0),
});

static xdo: XDo = XDo::new(None).expect("Failed to grab libXDo handle.");

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

// do_move is not tail-recursive! (fixable? / needed?)
fn do_tow(dx: i32, dy: i32) {
    let mut mq = get_move_queue();
    do_move(mq, dx, dy, 0.0, 0.0);
}
fn do_move(mut qu: Vec<f64>, mut dx: i32, mut dy: i32, mut cx: f64, mut cy: f64) {
    if !qu.is_empty() {
        let mut move_amount: f64;
        let y_as_xfraction: f64 = dy as f64 / dx as f64;

        if let Some(value) = qu.first() {
            move_amount = value * dx as f64 + cx; // cx times value?
            qu.remove(0);
        } else {
            println!("Wonderous machine!! do_tow failed in calculating move");
            return;
        }

        if move_amount < 1.0 {
            do_move(qu, dx, dy, move_amount, move_amount * y_as_xfraction);
        } else {
            let amount_y: i32 = (move_amount * y_as_xfraction.trunc()) as i32;
            xdo.move_mouse_relative(move_amount.trunc() as i32, amount_y)
                .expect("Failed to move!");
            dx -= move_amount.trunc() as i32;
            dy -= amount_y;
            do_move(
                qu,
                dx,
                dy,
                move_amount.fract(),
                move_amount * y_as_xfraction.fract(),
            );
            std::thread::sleep(FRAME_DUR);
        }
    }
}

// Disabled due to XDo from XDotool and XDo from -sys
/* fn get_pointer_coordinates(handle: XDo) -> (i32, i32) {
    // libxdo-sys
    let mut mouse_x = Vec::with_capacity(1 as usize);
    let pmouse_x = mouse_x.as_mut_ptr();
    let mut mouse_y = Vec::with_capacity(1 as usize);
    let pmouse_y = mouse_y.as_mut_ptr();

    let p_xdo = &handle as *const i32 as *const csize_t;

    unsafe {
        xdo_get_mouse_location(p_xdo, pmouse_x, pmouse_y, 0 as *mut ::libc::c_int);
    }
    // FIXME: Check for return != 0
    return (*pmouse_x as i32, *pmouse_y as i32);
} */

// ================= Foreign Types
type AtspiCache = u32;

type AtspiObject = _AtspiObject;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
struct _AtspiObject {
    parent: GObject,
    app: *const ::std::os::raw::c_void,
    path: *mut ::std::os::raw::c_char,
}

type AtspiAccessible = _AtspiAccessible;

#[repr(C)]
#[derive(Debug, Copy, Clone)]
struct _AtspiAccessible {
    parent: AtspiObject,
    accessible_parent: *mut AtspiAccessible,
    children: *mut GPtrArray,
    role: AtspiRole,
    interfaces: gint,
    name: *mut ::std::os::raw::c_char,
    description: *mut ::std::os::raw::c_char,
    states: *const ::std::os::raw::c_void, //AtspiStateSet
    attributes: *mut GHashTable,
    cached_properties: guint,
    priv_: *const ::std::os::raw::c_void, // AtspiAccessiblePrivate
}
type AtspiEventListenerCB = ::std::option::Option<
    unsafe extern "C" fn(event: *mut AtspiEvent, user_data: *mut ::std::os::raw::c_void),
>;
type AtspiRole = u32;

type AtspiEventListener = _AtspiEventListener;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
struct _AtspiEventListener {
    parent: GObject,
    callback: AtspiEventListenerCB,
    user_data: *mut ::std::os::raw::c_void,
    cb_destroyed: GDestroyNotify,
}
type AtspiLocaleType = u32;
type AtspiCoordType = u32;
const ATSPI_COORD_TYPE_SCREEN: AtspiCoordType = 0;
const ATSPI_COORD_TYPE_WINDOW: AtspiCoordType = 1;

type AtspiEvent = _AtspiEvent;
#[repr(C)]
#[derive(Copy, Clone)]
struct _AtspiEvent {
    type_: *mut gchar,
    source: *mut AtspiAccessible,
    detail1: gint,
    detail2: gint,
    any_data: GValue,
}
type AtspiText = _AtspiText;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
struct _AtspiText {
    parent: GTypeInterface,
}
type AtspiRect = _AtspiRect;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
struct _AtspiRect {
    x: gint,
    y: gint,
    width: gint,
    height: gint,
}
type AtspiEventListenerSimpleCB =
    ::std::option::Option<unsafe extern "C" fn(event: *const AtspiEvent)>;
type gint8 = ::std::os::raw::c_schar;
type guint8 = ::std::os::raw::c_uchar;
type gint16 = ::std::os::raw::c_short;
type guint16 = ::std::os::raw::c_ushort;
type gint32 = ::std::os::raw::c_int;
type guint32 = ::std::os::raw::c_uint;
type gint64 = ::std::os::raw::c_long;
type guint64 = ::std::os::raw::c_ulong;
type gssize = ::std::os::raw::c_long;
type gsize = ::std::os::raw::c_ulong;
type goffset = gint64;
type gintptr = ::std::os::raw::c_long;
type guintptr = ::std::os::raw::c_ulong;
type GPid = ::std::os::raw::c_int;

type GQuark = guint32;

//=========== End foreign types
//=========== Foreign Functions
#[link(name = "atspi")]
extern "C" {
    //--------- atspi_misc
    fn atspi_init() -> ::std::os::raw::c_int;
    fn atspi_exit() -> ::std::os::raw::c_int;
    fn atspi_event_main();
    fn atspi_event_quit();
    fn atspi_get_desktop_count() -> gint;

    //-------- atspi_text
    fn atspi_text_get_caret_offset(obj: *mut AtspiText, error: *mut *mut GError) -> gint;
    fn atspi_text_get_character_extents(
        obj: *mut AtspiText,
        offset: gint,
        type_: AtspiCoordType,
        error: *mut *mut GError,
    ) -> *mut AtspiRect;

    //------- atspi_accessible
    fn atspi_accessible_get_text(obj: *mut AtspiAccessible) -> *mut AtspiText;
    fn atspi_accessible_get_text_iface(obj: *mut AtspiAccessible) -> *mut AtspiText;

    //------- atsi_event
    fn atspi_event_listener_new_simple(
        callback: AtspiEventListenerSimpleCB,
        callback_destroyed: GDestroyNotify,
    ) -> *mut AtspiEventListener;

    fn atspi_event_listener_register(
        listener: *mut AtspiEventListener,
        event_type: *const gchar,
        error: *mut *mut GError,
    ) -> gboolean;

    fn atspi_event_listener_register_no_data(
        callback: AtspiEventListenerSimpleCB,
        callback_destroyed: GDestroyNotify,
        event_type: *const gchar,
        error: *mut *mut GError,
    ) -> gboolean;

}
//=========== End foreign functions

extern "C" fn on_caret_move(event: *const AtspiEvent) {
    use std::ptr::null_mut;
    let mut caret_offset: gtypes::gint = 0;

    let mut text_iface = unsafe { atspi_accessible_get_text_iface((*event).source) };
    caret_offset = unsafe { atspi_text_get_caret_offset(text_iface, null_mut()) };

    println!("Caret offset: {:?}", caret_offset);

    // Because we cannot ask for the co-ordinates of the caret directly,
    // we ask for the bounding box of the glyph at the position one before the carets.
    // (often there will not be a glyph at the carets position and we'll have more chance finding one at the preceding position)

    let cpos_bounds = unsafe {
        atspi_text_get_character_extents(
            text_iface,
            caret_offset - 1,
            ATSPI_COORD_TYPE_SCREEN,
            null_mut(), // GError
        )
    };

    // Dereferencing Raw in the println, unsafe
    unsafe {
        println!(
            "Caret position ({}, {})",
            (*cpos_bounds).x,
            (*cpos_bounds).y
        );
        let tup: (i32, i32) = ((*cpos_bounds).x, (*cpos_bounds).y);
        let mut current_cts = caret_tow_state.get();
        match current_cts.counter {
            0 => {
                current_cts.counter += 1;
                current_cts.first_pos = tup;
                current_cts.last_pos = tup;
                caret_tow_state.replace(current_cts);
            }
            1..=9 => {
                current_cts.counter += 1;
                current_cts.last_pos = tup;
                caret_tow_state.replace(current_cts);
            }
            10 => {
                current_cts.counter = 0;
                let dx: i32 = current_cts.last_pos.0 - current_cts.first_pos.0;
                let dy: i32 = current_cts.last_pos.1 - current_cts.first_pos.1;
                current_cts.first_pos = (0, 0);
                caret_tow_state.replace(current_cts);
                do_tow(dx, dy);
            }
            _ => {
                unreachable!();
            }
        };
    } // FIXME: Too long an unsafe block!
}

fn spookify_tow() {
    let stdout = File::create("/tmp/tow-daeomon.out").unwrap();
    let stderr = File::create("/tmp/tow-daemon.err").unwrap();

    let daemonize = Daemonize::new()
        .pid_file("/tmp/tow.pid") // Every method except `new` and `start`
        .chown_pid_file(true) // is optional, see `Daemonize` documentation
        .working_directory("/tmp") // for default behaviour.
        .user("nobody")
        .group("daemon") // Group name
        .group(2) // or group id.
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
    spookify_tow();

    let evfn: AtspiEventListenerSimpleCB = Some(on_caret_move);
    let ptr = CString::new("Hello").expect("CString::new failed").as_ptr();
    let towerror = unsafe { glib_sys::g_error_new(0, 0, ptr) };

    // AT-SPI event listeners
    let listener = unsafe { atspi_event_listener_new_simple(evfn, None) };
    // FIX: introduce proper error handling please

    // AT-SPI init
    if unsafe { atspi_init() } != 0 {
        eprintln!("Could not initialise AT-SPI.");
    }

    let evtype = CString::new("object:text-caret-moved")
        .expect("CString::new failed")
        .as_ptr();
    // let evtype = s.into_raw() as *const i8;
    let err: *mut *mut GError = std::ptr::null_mut();

    // let &mut zilch = null_mut::<GError>(&mut towerror); // Fixme into proper Error handling!

    unsafe {
        atspi_event_listener_register(listener, evtype, err);
    }

    unsafe {
        atspi_event_main();
    }

    /*     if unsafe { atspi_exit() } != 0 {
                    eprintln!("AT-SPI exit failed.");
    } */
}
