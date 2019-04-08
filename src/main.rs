/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
* file, You can obtain one at https://mozilla.org/MPL/2.0/. */

#![warn(clippy::all)]
#![feature(extern_types)]
#![allow(non_camel_case_types)]

/// Tow
/// an ergonomy utility for desktop zoom users.
/// tow has the zoom area be towed by the keyboard caret.
/// It is made with the Xfce4 desktop in mind,
/// however it might work with other desktop environments aswell.

#[macro_use]
extern crate clap;

use clap::{App, AppSettings, Arg};
use daemonize::Daemonize;
use glib_sys::{GDestroyNotify, GError};
use gtypes::gpointer;
use libc;
use std::f64::consts::E;
use std::ffi::CString;
use std::fs::File;
use std::ops::{Add, Sub};
use std::time::{Duration, Instant};
use xcb;
use xcb::base::Connection;

mod atspi_ffi;
use atspi_ffi::*;

static mut SLIDE_DUR: Duration = Duration::from_millis(500);
const FRAME_DUR: Duration = Duration::from_millis(1000 / 30);
const XCB_NONE: u32 = 0;

#[derive(Copy, Clone, Debug, PartialEq)]
struct Point(i32, i32);

impl Add for Point {
    type Output = Point;

    fn add(self, other: Point) -> Point {
        Point(self.0 + other.0, self.1 + other.1)
    }
}
impl Sub for Point {
    type Output = Point;

    fn sub(self, other: Point) -> Point {
        Point(self.0 - other.0, self.1 - other.1)
    }
}

#[derive(Clone, Copy, Debug)]
struct CaretTowState {
    counter: u64, // CHANGE INTO WRAPPING TYPE?!
    timestamp: Option<Instant>,
    pointer_caret_offset: (i32, i32), // an offset is semantically not a point
    pointer_at_begin: Option<Point>,
    glyph_coords_begin: Option<Point>,
    behavior: Behavior,
}
impl CaretTowState {
    fn reset(&mut self) {
        self.counter = 0;
        self.timestamp = Some(Instant::now());
        self.pointer_at_begin = None;
        self.glyph_coords_begin = None;
    }
    fn advance(&mut self, l: Point) {
        self.counter += 1;
        self.glyph_coords_begin = Some(l);
    }
    fn pointer_offset(&mut self) {
        self.pointer_caret_offset = (
            self.glyph_coords_begin
                .expect("no glyph begin coords found")
                .0
                - self.pointer_at_begin.expect("no pointer coords found").0,
            self.glyph_coords_begin
                .expect("no glyph begin coords found")
                .1
                - self.pointer_at_begin.expect("no pointer coords found").1,
        );
    }
}

#[derive(Clone, Copy, Debug)]
enum Behavior {
    Interval { dur: Duration },
    PerQty { nc: u32 },
    Typewriter,
}

//  .map(|x| (E.powf(x) - E.powf(-x)) / (E.powf(x) + E.powf(-x))) // tanh
// get_sigmoid makes us a S-shaped graph data
// a fluent slope within certain bounds
fn get_sigmoid() -> Vec<f64> {
    let frames_per_slide = unsafe {
        ((SLIDE_DUR.as_secs() * 1000_u64 + SLIDE_DUR.subsec_millis() as u64)
            / (FRAME_DUR.as_secs() * 1000_u64 + FRAME_DUR.subsec_millis() as u64)) as f64
    };
    let dx: f64 = 1.0 / (frames_per_slide * 0.5);
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
    let mut cl = right_half.clone();
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

fn do_tow(dx: i32, dy: i32, conn: &Connection, screen_num: i32, begin: Point) {
    let mut values = get_move_shape();
    let nframes = values.len();
    let mut cx: f64 = 0.0;
    let mut xmove_amount: f64 = 0.0;
    let mut ymove_amount: f64 = 0.0;
    let y_in_x: f64 = dy as f64 / dx as f64;
    let mut x = begin.0;
    let mut y = begin.1;

    while !values.is_empty() {
        if let Some(value) = values.pop() {
            xmove_amount = (value * dx as f64 + cx as f64) / (nframes as f64 / 2.0) as f64;
        } else {
            panic!("First value in values appears to not be Some().");
        }

        if xmove_amount.abs() < 1.0 {
            cx = xmove_amount;
            continue; // next iteration of while
        } else if xmove_amount.abs() >= 1.0 {
            ymove_amount = xmove_amount * y_in_x;
            warp_abs(
                x + xmove_amount.trunc() as i32,
                y + ymove_amount.trunc() as i32,
                conn,
                screen_num,
            );
            x += xmove_amount.trunc() as i32;
            y += ymove_amount.trunc() as i32;
            cx = xmove_amount.fract();
            std::thread::sleep(FRAME_DUR);
        } else {
            panic!("Wonderous machine! xmove_amount abnormal.");
        }
    }
}

fn warp_abs(x: i32, y: i32, conn: &Connection, screen_num: i32) {
    let setup = conn.get_setup();
    let screen = setup
        .roots()
        .nth(screen_num as usize)
        .expect("Can't get screen.");
    let root_id = screen.root();
    xcb::warp_pointer(
        conn, XCB_NONE, root_id, 0 as i16, 0 as i16, 0 as u16, 0 as u16, x as i16, y as i16,
    )
    .request_check()
    .expect("Warp pointer check failed?");
}

fn warp_rel(x: i32, y: i32, conn: &Connection) {
    xcb::warp_pointer(
        conn, XCB_NONE, XCB_NONE, 0 as i16, 0 as i16, 0 as u16, 0 as u16, x as i16, y as i16,
    )
    .request_check()
    .expect("Check in warp-rel failed!");
}

fn get_pointer_coordinates(conn: &Connection, screen_num: i32) -> Point {
    let setup = conn.get_setup();
    let screen = setup
        .roots()
        .nth(screen_num as usize)
        .expect("Cannot get screen!");
    let root_id = screen.root();

    let pointercookie = xcb::xproto::query_pointer(&conn, root_id);

    match pointercookie.get_reply() {
        Ok(r) => {
            return Point(r.root_x() as i32, r.root_y() as i32);
        }
        Err(_) => {
            panic!("could not get coordinates of pointer");
        }
    };
}

extern "C" fn destroy_evgarbage(data: gpointer) {
    unsafe { libc::free(data) };
}

extern "C" fn on_caret_move(event: *mut AtspiEvent, voidptr_data: *mut ::std::os::raw::c_void) {
    use std::ptr::null_mut;

    // The pointer was a borrow &mut, when the listener took it.
    // We cannot eat a lent cake, so we cannot dereference the pointer.

    let pdata = voidptr_data as *mut (CaretTowState, xcb::base::Connection, i32);

    let text_iface = unsafe { atspi_accessible_get_text_iface((*event).source) };
    let caret_offset = unsafe { atspi_text_get_caret_offset(text_iface, null_mut()) };

    // Because we cannot ask for the coordinates of the caret directly,
    // we ask for the bounding box of the glyph at the caret offset.
    let glyph_extents = unsafe {
        atspi_text_get_character_extents(
            text_iface,
            caret_offset,
            ATSPI_COORD_TYPE_SCREEN,
            null_mut(), // GError
        )
    };

    let caret_coords_now = unsafe { Point((*glyph_extents).x as i32, (*glyph_extents).y as i32) };
    if caret_coords_now == Point(0, 0) {
        return;
    }

    // Rust will assure we'll never consume the lent cake, however we are allowed to destructure it!

    let (cts_curr, conn, screen_num) = unsafe { pdata.as_mut().expect("NULL pointer passed!") };

    if !cts_curr.glyph_coords_begin.is_some() {
        cts_curr.glyph_coords_begin = Some(caret_coords_now)
    }

    let pointer_coordinates: Point = get_pointer_coordinates(conn, *screen_num);

    let periodically = |dur, cts_curr: &mut CaretTowState| {
        if cts_curr.pointer_at_begin.expect("cts: no pointer begin") != pointer_coordinates {
            cts_curr.reset();
            cts_curr.pointer_at_begin = Some(pointer_coordinates);
            cts_curr.glyph_coords_begin = Some(caret_coords_now);
            return;
        }

        match cts_curr.timestamp {
            Some(timestamp) if timestamp.elapsed() < dur => {
                return;
            }
            Some(timestamp) if timestamp.elapsed() >= dur => {
                do_tow(
                    caret_coords_now.0
                        - cts_curr.glyph_coords_begin.expect("cts: no glyph begin").0,
                    caret_coords_now.1
                        - cts_curr.glyph_coords_begin.expect("cts: no glyph begin").1,
                    conn,
                    *screen_num,
                    cts_curr.glyph_coords_begin.expect("cts: no glyph begin"),
                );
                cts_curr.reset();
                cts_curr.pointer_at_begin = Some(pointer_coordinates);
                cts_curr.glyph_coords_begin = Some(caret_coords_now);
                return;
            }
            None => {
                cts_curr.reset();
                cts_curr.pointer_at_begin = Some(pointer_coordinates);
                cts_curr.glyph_coords_begin = Some(caret_coords_now);
                return;
            }
            Some(_) => {
                panic!("Wonderous machine: unreachable state timestamp value");
            }
        };
    };

    // 'Move per X events is fine, until:
    // - somewhere between 0..X the pointer is moved
    // and with it the view port.

    let every_n_characters = |nc, cts_curr: &mut CaretTowState| {
        if cts_curr.pointer_at_begin.expect("cts: no pointer begin") != pointer_coordinates {
            // We dared to move the pointer tssk
            cts_curr.reset();
            cts_curr.pointer_at_begin = Some(pointer_coordinates);
            cts_curr.glyph_coords_begin = Some(caret_coords_now);
            return;
        }

        if cts_curr.counter == 0 {
            cts_curr.counter = 1;
            cts_curr.pointer_at_begin = Some(pointer_coordinates);
            cts_curr.glyph_coords_begin = Some(caret_coords_now);
            cts_curr.pointer_offset();
            return;
        } else if cts_curr.counter > 0 && cts_curr.counter < nc {
            cts_curr.advance(caret_coords_now);
            return;
        } else {
            do_tow(
                caret_coords_now.0 - cts_curr.glyph_coords_begin.expect("cts: no glyph begin").0,
                caret_coords_now.1 - cts_curr.glyph_coords_begin.expect("cts: no glyph begin").1,
                conn,
                *screen_num,
                cts_curr.glyph_coords_begin.expect("cts: no glyph begin"),
            );
            cts_curr.reset();
            cts_curr.pointer_at_begin = Some(pointer_coordinates);
            cts_curr.glyph_coords_begin = Some(caret_coords_now);
            return;
        }
    };

    let each_character = |cts_curr: &mut CaretTowState| {
        let x = caret_coords_now.0 - 12;
        let y = caret_coords_now.1 - 15;
        warp_abs(x, y, conn, *screen_num);
        return;
    };

    match cts_curr.behavior {
        Behavior::Interval { dur } => periodically(dur, cts_curr),
        Behavior::PerQty { nc } => every_n_characters(nc as u64, cts_curr),
        Behavior::Typewriter => each_character(cts_curr),
    };
}

fn spookify_tow() {
    let stdout = File::create("/tmp/tow-daemon.out").expect("daemonize: cannot creat stdout file");
    let stderr = File::create("/tmp/tow-daemon.err").expect("daemonize: cannot creat stderr file");

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
    // Passed around state
    let mut cts: CaretTowState = CaretTowState {
        counter: 0,
        timestamp: Some(std::time::Instant::now()),
        pointer_caret_offset: (0, 0),
        pointer_at_begin: None,
        glyph_coords_begin: None,
        behavior: Behavior::Typewriter,
    };

    let matches = App::new("Tow")
        .version(crate_version!())
        .author("Luuk van der Duim <luukvanderduim@gmail.com>")
        .arg(
            Arg::with_name("daemon")
                .short("D")
                .long("daemon")
                .takes_value(false)
                .help("Have tow be 'daemonized' / run in the background."),
        )
        .arg(
            Arg::with_name("slidedur")
                .short("s")
                .long("slide_duration")
                .takes_value(true)
                .default_value("500")
                .help("Duration of view port slide in ms [100-10000] only applies to charcnt and interval modes, otherwise ignored"),
        )
        .arg(Arg::with_name("behavior")
            .short("b")
            .long("behavior")
            //.default_value("typewriter")
            .takes_value(true)
            .help("Mode: charcnt [N:2-100] (# chars), interval [N: 100-10000] (ms) or typewriter (default)")
            .max_values(2))
        .get_matches();

    if matches.is_present("daemon") {
        spookify_tow();
    }

    if matches.is_present("behavior") {
        let mut bvals = matches.values_of("behavior").expect("Unexpected!");
        match bvals.next() {
            Some("charcnt") => {
                if let Some(numb) = bvals.next() {
                    let n = numb.parse::<u8>().expect("u8 parse error");
                    if n <= 1 {
                        cts.behavior = Behavior::Typewriter;
                    } else if n > 1 && n <= 100 {
                        cts.behavior = Behavior::PerQty { nc: n as u32 };
                    } else {
                        cts.behavior = Behavior::PerQty { nc: 100 };
                    }
                }
            }
            Some("interval") => {
                if let Some(numb) = bvals.next() {
                    let n = numb.parse::<u16>().expect("u16 parse error");
                    if n <= 99 {
                        cts.behavior = Behavior::Interval {
                            dur: Duration::from_millis(100),
                        };
                    }
                    if n >= 100 && n <= 10000 {
                        cts.behavior = Behavior::Interval {
                            dur: Duration::from_millis(n as u64),
                        };
                    } else {
                        cts.behavior = Behavior::Interval {
                            dur: Duration::from_millis(10000),
                        };
                    }
                }
            }
            Some("typewriter") => {
                cts.behavior = Behavior::Typewriter;
            }
            Some(&_) => {
                eprintln!("Error: Invalid behavior value. Typo?");
            }
            None => {
                eprintln!("Error: Invalid behavior value.");
            }
        }

        if let Some(s) = matches.value_of("slidedur") {
            let val: u64 = s.parse::<u64>().expect("Not a valid duration (see -h).");
            if val <= 10000 && val >= 100 {
                unsafe { SLIDE_DUR = Duration::from_millis(val) };
            }
        }
    }

    let (conn, screen_num) = xcb::Connection::connect(None).expect("Failed xcb connection.");
    cts.pointer_at_begin = Some(get_pointer_coordinates(&conn, screen_num));

    let mut triplet = (cts, conn, screen_num);

    let voidptr_data =
        &mut triplet as *mut (CaretTowState, xcb::base::Connection, i32) as *mut libc::c_void;
    let evfn: AtspiEventListenerCB = Some(on_caret_move);
    let post_event_chores: GDestroyNotify = Some(destroy_evgarbage);

    // AT-SPI event listener
    let listener = unsafe { atspi_event_listener_new(evfn, voidptr_data, post_event_chores) };

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
        gobject_sys::g_object_unref(err as *mut gobject_sys::GObject); // as GError
        gobject_sys::g_object_unref(evtype as *mut gobject_sys::GObject); // as
        gobject_sys::g_object_unref(listener as *mut gobject_sys::GObject);
        gobject_sys::g_object_unref(voidptr_data as *mut gobject_sys::GObject);
    }
}
