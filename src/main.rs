/*   Copyright 2019 Luuk van der Duim

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
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
tow: an Xfce desktop zoom ergonomy helper.
tow has the zoom area be towed by the keyboard caret!
*/

#![feature(duration_as_u128)]
#![feature(extern_types)]
#![allow(unused_imports)]

use glib;
use gtypes;
use kahan::{KahanSum, KahanSummator};
use libc;
use libxdo::XDo;
use libxdo_sys::*;
use std::f64::consts::E;
use std::ptr::null;
use std::thread;
use std::time::{Duration, Instant};
use std::os::raw::c_char;
use atspisys; //FIXME make bindings that do not include the world

const SLIDE_DUR: Duration = Duration::from_micros(1_000_000);
const FRAME_DUR: Duration = Duration::from_micros(1_000_000 / 30);

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

// do_move is not tail-recursive! (fixable? / needed?)
fn do_move(xdo: XDo, mut qu: Vec<f64>, mut dx: u64, c: f64) {
    if !qu.is_empty() {
        std::thread::sleep(FRAME_DUR);
        let mut move_amount: f64;

        if let Some(value) = qu.first() {
            move_amount = value * dx as f64 + c;
            qu.remove(0);
        } else {
            println!("Wonderous machine!!");
            return;
        }

        if move_amount < 1.0 {
            do_move(xdo, qu, dx, move_amount)
        } else {
            xdo.move_mouse_relative(move_amount.trunc() as i32, 0)
                .expect("Failed to move!");
            dx -= move_amount.trunc() as u64;
            do_move(xdo, qu, dx, move_amount.fract());
        }
    }
}

fn get_pointer_coordinates(xdo: XDo) -> (i32, i32) {
    // libxdo-sys
    unsafe {
        let mut mouse_x = Vec::with_capacity(1 as usize);
        let pmouse_x = mouse_x.as_mut_ptr();
        let mut mouse_y = Vec::with_capacity(1 as usize);
        let pmouse_y = mouse_y.as_mut_ptr();

        if xdo_get_mouse_location(xdo, pmouse_x, pmouse_y, 0 as *mut ::libc::c_int) != 0 {
            panic!("Failed to get (mouse) pointer location.");
        }
        return (*pmouse_x as i32, *pmouse_y as i32);
    }
}

#[link(name = "atspi")]
extern {

type AtspiAccessible = _AtspiAccessible;

#[repr(C)]
#[derive(Debug, Copy, Clone)]
 struct _AtspiAccessiblePrivate {
    _unused: [u8; 0],
}
 type AtspiAccessiblePrivate = _AtspiAccessiblePrivate;
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
     states: *mut AtspiStateSet,
     attributes: *mut GHashTable,
     cached_properties: guint,
     priv_: *mut AtspiAccessiblePrivate,
}

type AtspiStateSet = _AtspiStateSet;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
 struct _AtspiStateSet {
     parent: GObject,
     accessible: *mut _AtspiAccessible,
     states: gint64,
}

type AtspiRole = u32;

type AtspiEventListenerSimpleCB =
    ::std::option::Option<unsafe extern "C" fn(event: *const AtspiEvent)>;

 type AtspiEventListener = _AtspiEventListener;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
 struct _AtspiEventListener {
     parent: GObject,
     callback: AtspiEventListenerCB,
     user_data: *mut ::std::os::raw::c_void,
     cb_destroyed: GDestroyNotify,
}

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

    // atspi_misc
 fn atspi_init() -> ::std::os::raw::c_int;
 fn atspi_exit() -> ::std::os::raw::c_int;
    fn atspi_event_main();
    fn atspi_event_quit();
 fn atspi_get_desktop_count() -> gint;

    // atspi_text
    fn atspi_text_get_caret_offset(obj: *mut AtspiText, error: *mut *mut GError) -> gint;
 fn atspi_text_get_character_extents(
        obj: *mut AtspiText,
        offset: gint,
        type_: AtspiCoordType,
        error: *mut *mut GError,
    ) -> *mut AtspiRect;

    // atspi_accessible
 fn atspi_accessible_get_text(obj: *mut AtspiAccessible) -> *mut AtspiText;
 fn atspi_accessible_get_text_iface(obj: *mut AtspiAccessible) -> *mut AtspiText;

    // atsi_event
 fn atspi_event_listener_new_simple(
    callback: AtspiEventListenerSimpleCB,
    callback_destroyed: GDestroyNotify,
    ) -> *mut AtspiEventListener;

type AtspiEventListenerSimpleCB =
    ::std::option::Option<unsafe extern "C" fn(event: *const AtspiEvent)>;

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

extern fn on_caret_move(event: *const AtspiEvent) {
    // on_caret_ev(AtspiEvent *event)
  unsafe {
  type ptrAtspiAccessible = *mut AtspiAccessible;
  type ptrAtspiText = *mut AtspiText;
  type ptrAtspiRect = *mut AtspiRect;
  let mut application: ptrAtspiAccessible = AtspiAccessible;
  let mut app_name: gtypes::gchar = "".as_ptr();
  let mut text_iface: ptrAtspiText = *mut std::ptr::null();
  let mut cpos_bounds: ptrAtspiRect = *mut std::ptr::null();
  }
  let mut caret_offset: gtypes::gint = 0;

  text_iface = atspi_accessible_get_text_iface(event->source);
  if text_iface.is_null()
  {
    g_print("Wonderous machine: event happened, yet caret not in a text component\n");
    g_print("Accessible: %s\nDescription: %s\n", atspi_accessible_get_name(event->source, NULL),
            atspi_accessible_get_description(event->source, NULL));
  }

  caret_offset = atspi_text_get_caret_offset(text_iface, NULL);
  g_print("Caret offset: %d\n", caret_offset);

  // Because we cannot ask for the co-ordinates of the caret directly,
  // we ask for the bounding box of the glyph at the position one before the carets.
  // (often there will not be a glyph at the carets position and we'll have more chance finding one at the preceding position)
  cpos_bounds = atspi_text_get_character_extents(text_iface, --caret_offset, ATSPI_COORD_TYPE_SCREEN, NULL);
  g_print("Caret position (%d, %d)\n", cpos_bounds->x, cpos_bounds->y);

}


fn main() {

let AtspiEventListener

    // AT-SPI event listeners
    unsafe {
        let listener: plistener = atspi_event_listener_new_simple(on_caret_move, std::ptr::null());
        // FIXME: Error handling
    }

    // AT-SPI init
    if unsafe { atspisys::atspi_init() } != 0 {
        eprintln!("Could not initialise AT-SPI.");
    }

    let x1: i32 = 0;
    let x2: i32 = 300;
    let distance: u64 = (x2 - x1).abs() as u64;

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

    let xdo = XDo::new(None).expect("Failed to grab libXDo/X11 handle.");
    do_move(xdo, queue, distance, 0.0);




    if unsafe { atspisys::atspi_exit() } != 0 {
        eprintln!("AT-SPI exit failed.");
    }
}
