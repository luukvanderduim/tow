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
//#![warn(clippy)#![warn(clippy)]

#![feature(duration_as_u128)]
#![feature(extern_types)]
// #![allow(unused_imports)]
#![allow(non_camel_case_types)]

use daemonize::Daemonize;
use libc;
//use glib::error::Error;
//use glib::Quark;
use glib_sys::{GDestroyNotify, GError, GHashTable, GPtrArray};
use gobject_sys::*;
use gtypes::primitive::{gboolean, gchar, gint, guint};
use kahan::{KahanSum, KahanSummator};
use libxdo::XDo;
use libxdo_sys::*;
use std::f64::consts::E;
use std::ffi::CString;
use std::fs::File;
use std::sync::{Arc, Mutex};
use std::time::Duration;

const SLIDE_DUR: Duration = Duration::from_micros(1_000_000);
const FRAME_DUR: Duration = Duration::from_micros(1_000_000 / 30);

#[derive(Clone, Copy, Debug)]
struct CaretTowState {
    counter: i32,
    pointer_first: (i32, i32),
    first_glyph_coords: (i32, i32),
    last_glyph_coords: (i32, i32),
}
impl CaretTowState {
    fn reset(&mut self) {
        self.counter = 0;
        self.pointer_first = (0, 0);
        self.first_glyph_coords = (0, 0);
        self.last_glyph_coords = (0, 0);
    }
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
    let xdo = libxdo::XDo::new(None).expect("Failed to obtain XDo handle.");
    let mq = get_move_queue();
    // do_move(xdo, mq, dx, dy, 0.0);

    xdo.move_mouse_relative(dx, dy).expect("Failed to move!");
    /*     unsafe {
        xdo.move_mouse(
            CARET_TOW_STATE.get().last_glyph_coords.0,
            CARET_TOW_STATE.get().last_glyph_coords.1,
            0,
        )
    }; */
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

fn get_pointer_coordinates(handle: libxdo_sys::Struct_xdo) -> (i32, i32) {
    let mut mouse_x = Vec::with_capacity(1 as usize);
    let pmouse_x = mouse_x.as_mut_ptr();
    let mut mouse_y = Vec::with_capacity(1 as usize);
    let pmouse_y = mouse_y.as_mut_ptr();

    unsafe {
        xdo_get_mouse_location(
            &handle as *const xdo_t,
            pmouse_x,
            pmouse_y,
            0 as *mut ::libc::c_int,
        );
    }
    debug_assert!(!pmouse_x.is_null());
    debug_assert!(!pmouse_y.is_null());

    let x: i32 = unsafe { *pmouse_x as i32 };
    let y: i32 = unsafe { *pmouse_y as i32 };

    unsafe {
        libc::free(pmouse_x as *mut std::ffi::c_void);
        libc::free(pmouse_y as *mut std::ffi::c_void);
    }
    (x, y)
}

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
    detail1: gint, // Registered events mask (?)
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
type gpointer = *mut ::std::os::raw::c_void;

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

    fn atspi_event_listener_new(
        callback: AtspiEventListenerCB,
        user_data: gpointer,
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

extern "C" fn destroy_evgarbage(data: gpointer) {
    unsafe { libc::free(data) };
}

extern "C" fn on_caret_move(event: *mut AtspiEvent, vpdata: *mut ::std::os::raw::c_void) {
    use std::ptr::null_mut;

    // coupled: Box<<(Arc<Mutex<CaretTowState>>, *mut libxdo_sys::Struct_xdo)>>
    let coupled = unsafe {
        Box::from_raw(
            vpdata
                as *mut Box<(
                    Arc<Mutex<CaretTowState>>,
                    Arc<Mutex<*mut libxdo_sys::Struct_xdo>>,
                )>,
        )
    };

    let text_iface = unsafe { atspi_accessible_get_text_iface((*event).source) };
    assert!(!text_iface.is_null());

    let mut caret_offset = unsafe { atspi_text_get_caret_offset(text_iface, null_mut()) };

    /* 'Detail1' is likely the mask of event(s) we registered for (?) */
    if unsafe { ((*event).detail1 == 0) } {
        unsafe {
            gobject_sys::g_object_unref(event as *mut gobject_sys::GObject);
            gobject_sys::g_object_unref(text_iface as *mut gobject_sys::GObject);
        }
        return;
    }

    // Because we cannot ask for the co-ordinates of the caret directly,
    // we ask for the bounding box of the glyph at the position one before the carets.
    // (often there will not be a glyph at the carets position and we'll have more
    // chance finding one at the preceding position) */
    let glyph_extents = unsafe {
        atspi_text_get_character_extents(
            text_iface,
            caret_offset - 1,
            ATSPI_COORD_TYPE_SCREEN,
            null_mut(), // GError
        )
    };
    debug_assert!(!glyph_extents.is_null());

    // Often atspi_text_get_character_extents() seems to return twice:
    // Once with a valid value and once, supposedly at the origin of the screen
    // Some applications suffer from this, others dont. (Bluefish doesnt, xed does)
    // Until the cause is found and fixed, filter these.

    let caret_coords_now: (i32, i32) = unsafe { ((*glyph_extents).x, (*glyph_extents).y) };
    if caret_coords_now == (0, 0) {
        unsafe {
            gobject_sys::g_object_unref(event as *mut gobject_sys::GObject);
            gobject_sys::g_object_unref(glyph_extents as *mut gobject_sys::GObject);
            gobject_sys::g_object_unref(text_iface as *mut gobject_sys::GObject);
        }
        return;
    }

    // 'Move per ten events'is fine as stategy until:
    // - somewhere between 0and 10 the mouse pointer is moved
    // and with it our view port.

    // Split data tuple in its constituents
    let c_cts_curr = (*coupled).0.clone();
    let c_pxdo_sys = (*coupled).1.clone(); // pxdo_sys is cleaned up with GDestroyNotify(?)

    let mut pxdo_sys = c_pxdo_sys.lock().unwrap();
    let mut curr_cts = c_cts_curr.lock().unwrap();

    let pointer_coordinates = unsafe { get_pointer_coordinates(**pxdo_sys) };

    match (*curr_cts).counter {
        0 => {
            println!("match 0, counter: {:?}", (*curr_cts).counter);
            (*curr_cts).counter += 1;
            (*curr_cts).pointer_first = pointer_coordinates;
            (*curr_cts).first_glyph_coords = caret_coords_now;
            (*curr_cts).last_glyph_coords = caret_coords_now;
            unsafe {
                gobject_sys::g_object_unref(event as *mut gobject_sys::GObject);
                gobject_sys::g_object_unref(glyph_extents as *mut gobject_sys::GObject);
                gobject_sys::g_object_unref(text_iface as *mut gobject_sys::GObject);
            }
            return;
        }
        1..=9 => {
            println!("match 1..=9, counter: {:?}", (*curr_cts).counter);
            if (*curr_cts).pointer_first != pointer_coordinates {
                (*curr_cts).reset();
                unsafe {
                    gobject_sys::g_object_unref(event as *mut gobject_sys::GObject);
                    gobject_sys::g_object_unref(glyph_extents as *mut gobject_sys::GObject);
                    gobject_sys::g_object_unref(text_iface as *mut gobject_sys::GObject);
                }
                return;
            }
            (*curr_cts).counter += 1;
            (*curr_cts).last_glyph_coords = caret_coords_now;

            unsafe {
                gobject_sys::g_object_unref(event as *mut gobject_sys::GObject);
                gobject_sys::g_object_unref(glyph_extents as *mut gobject_sys::GObject);
                gobject_sys::g_object_unref(text_iface as *mut gobject_sys::GObject);
            }
            return;
        }
        _ => {
            println!("match 10, counter: {:?}", (*curr_cts).counter);
            if (*curr_cts).pointer_first != pointer_coordinates {
                (*curr_cts).reset();
                unsafe {
                    gobject_sys::g_object_unref(event as *mut gobject_sys::GObject);
                    gobject_sys::g_object_unref(glyph_extents as *mut gobject_sys::GObject);
                    gobject_sys::g_object_unref(text_iface as *mut gobject_sys::GObject);
                }
                return;
            }
            let dx: i32 = (*curr_cts).last_glyph_coords.0 - (*curr_cts).first_glyph_coords.0;
            let dy: i32 = (*curr_cts).last_glyph_coords.1 - (*curr_cts).first_glyph_coords.1;
            (*curr_cts).reset();

            do_tow(dx, dy);
        }
    };
    unsafe {
        gobject_sys::g_object_unref(event as *mut gobject_sys::GObject);
        gobject_sys::g_object_unref(glyph_extents as *mut gobject_sys::GObject);
        gobject_sys::g_object_unref(text_iface as *mut gobject_sys::GObject);
    }
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

    // Shared state between CBs
    let mut cts: Arc<Mutex<CaretTowState>> = Arc::new(Mutex::new(CaretTowState {
        counter: 0,
        pointer_first: (0, 0),
        first_glyph_coords: (0, 0),
        last_glyph_coords: (0, 0),
    }));

    // xdo_sys pointer
    let xdo_sys_ptr = unsafe { libxdo_sys::xdo_new(std::ptr::null()) };
    debug_assert!(!xdo_sys_ptr.is_null());
    let pxdo_sys = Arc::new(Mutex::new(xdo_sys_ptr));

    // Two into one arg.
    let data = Box::new((cts, pxdo_sys));
    let vpdata = Box::into_raw(data) as *mut libc::c_void;

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
