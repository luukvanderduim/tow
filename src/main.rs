/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
* file, You can obtain one at https://mozilla.org/MPL/2.0/. */

#![warn(clippy::all)]
#![feature(extern_types)]
#![allow(non_camel_case_types)]

/// Tow
///
/// An convenience helper program for desktop zoom users.
/// Tow has the zoom area be towed by the keyboard caret.
/// It is made for the Xfce4 desktop, but not bound to.
/// It might work with other desktop environments aswell.

#[macro_use]
extern crate clap;

use clap::{App, Arg};

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
use xcb::ffi::base::XCB_NONE;

mod atspi_ffi;
use atspi_ffi::*;

// static mut SLIDE_DUR: Duration = Duration::from_millis(500);
const SLIDE_DUR: Duration = Duration::from_millis(500);
const FRAME_CALC: u64 = (1000.0 / 30.0) as u64;
const FRAME_DUR: Duration = Duration::from_millis(FRAME_CALC);
const BIG: f64 = 1000.0;

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
    accessible_id: Option<i32>,
    counter: u32,
    mvset: bool,
    timestamp: Option<Instant>,
    pointer_caret_offset: (i32, i32), // an offset is semantically not a point
    pointer_at_begin: Option<Point>,
    glyph_coords_begin: Option<Point>,
    behavior: Behavior,
}
impl CaretTowState {
    fn reset(&mut self) {
        self.accessible_id = None;
        self.counter = 0;
        self.mvset = false;
        self.timestamp = Some(Instant::now());
        self.pointer_at_begin = None;
        self.glyph_coords_begin = None;
    }

    fn pointer_caret_offset(&mut self) {
        self.pointer_caret_offset = (
            self.pointer_at_begin.unwrap().0
                - self.glyph_coords_begin.expect("no glyph coords found").0,
            self.pointer_at_begin.expect("no glyph begin").1
                - self.glyph_coords_begin.expect("no glyph coords found").1,
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

fn get_sigmoid_shape() -> Vec<f64> {
    // Slide in ms / Frame in ms = frames / slide
    let frames_per_slide = ((SLIDE_DUR.as_secs() * 1000_u64 + SLIDE_DUR.subsec_millis() as u64)
        / (FRAME_DUR.as_secs() * 1000_u64 + FRAME_DUR.subsec_millis() as u64))
        as f64;

    let dx: f64 = 1.0 / (frames_per_slide / 2.0);
    let sigma: Vec<f64> = (0..1000)
        .step_by((f64::from(dx * 1000.0)) as usize)
        .map(|x| f64::from(x) / f64::from(1000))
        .map(|x| f64::from(1.0) / f64::from(1.0 + E.powf(12.0 * x - 6.0)))
        .collect();

    let innate_sum = sigma.iter().sum::<f64>();

    // FIRST multiply by BIG number ONLY THEN normalize
    //
    let sigm: Vec<f64> = sigma
        .into_iter()
        .map(|x| x * BIG)
        .map(|x| x / innate_sum)
        .map(|x| x / 2.0)
        .collect();

    // shape yields a _/\_ shaped curve
    // shape contains two sigmoids of which the first is reversed.

    let mut shape: Vec<f64> = sigm.clone().into_iter().rev().collect(); //clone needed!
    shape.append(&mut sigm.clone());

    shape
}

#[cfg(test)]
#[test]
fn test_get_sigmoid_shape() {
    let n = get_sigmoid_shape().len();
    let mut shape = get_sigmoid_shape();
    for _ in 0..(n / 2) {
        assert_eq!(shape.first(), shape.last());
        shape.pop();
        shape.remove(0);
    }
}

fn do_tow(deltax: i32, deltay: i32, conn: &Connection, screen_num: i32, mut begin: Point) -> Point {
    let shapevalues: Vec<f64> = get_sigmoid_shape();
    let len = shapevalues.len();
    let threshold = BIG / len as f64;

    // xmove in amount of whole pIXels to move
    let mut xmove_pixels: f64;
    let mut ymove_pixels: f64;

    // Subpixel amount carry to next iteration
    let mut cx: f64 = 0.0;
    let mut cy: f64 = 0.0;

    // Depart from x and y in iteration(n)
    let mut x: f64;
    let mut y: f64;

    // To avoid calling xcb with nothing to do,
    // Leave a crumb with the last call values
    let mut crumb: Option<(f64, f64)> = None;

    for (i, v) in shapevalues.into_iter().enumerate() {
        // During this frame, the amount of pixels to move is:
        xmove_pixels = v * deltax as f64;
        ymove_pixels = v * deltay as f64;

        //
        //  X:
        //

        if xmove_pixels.abs() >= threshold {
            //  x moves this frame by:
            x = begin.0 as f64 + ((xmove_pixels + cx) / BIG) as f64;

            //  The fraction is carried to the next iteration
            cx = x.fract() * BIG;

            //  Next iterations x starting point
            begin.0 = x.trunc() as i32;
        } else {
            //  Smaller than the threshold, carry all
            cx += xmove_pixels;

            //  Nevertheless set y for the move
            x = begin.0 as f64;
        }

        // Bonus squeeze
        // last iteration left us with a remainder, we need to process this
        if i == len - 1 {
            x = begin.0 as f64 + (cx / BIG).round() as f64;
            begin.0 = x as i32;
        }

        //
        //  Y:
        //

        if ymove_pixels.abs() >= threshold {
            //  y moves this frame by:
            y = begin.1 as f64 + ((ymove_pixels + cy) / BIG) as f64;
            //  The fraction is carried to the next iteration
            cy = y.fract() * BIG;

            //  Next iterations x starting point
            begin.1 = y.trunc() as i32;
        } else {
            //  Smaller than the threshold, carry all
            cy += ymove_pixels;

            //  Nevertheless set y for the move
            y = begin.1 as f64;
        }

        // === Bonus squeeze
        // last iteration left us with a remainder, we need to process this
        if i == len - 1 {
            y = begin.1 as f64 + (cy / BIG).round() as f64;
            begin.1 = y as i32;
        }

        // === Crumb tasting!
        // Did last iteration leave a crumb?
        // Taste it to evaluate if we are about to do the same
        // To avoid calling xcb without a cause!
        if let Some(cr) = crumb {
            if cr == (x, y) {
                std::thread::sleep(FRAME_DUR);
            } else {
                warp_abs(x as i32, y as i32, conn, screen_num);
                std::thread::sleep(FRAME_DUR);
                crumb = Some((x, y));
            }
        } else {
            warp_abs(x as i32, y as i32, conn, screen_num);
            std::thread::sleep(FRAME_DUR);
            crumb = Some((x, y));
        }
    } // end delimiter of for loop
    return begin;
}

#[cfg(test)]
#[test]
fn test_do_tow() {
    use rand::{thread_rng, Rng};

    let mut rng = thread_rng();

    let (conn, screen_num) = xcb::Connection::connect(None).expect("Failed xcb connection.");
    for _ in 0..10 {
        let p: Point = Point(rng.gen_range(-1001, 1001), rng.gen_range(-1001, 1001));
        let q: Point = Point(rng.gen_range(0, 1921), rng.gen_range(0, 1080));
        let ans = p + q;
        let r: Point = do_tow(q.0, q.1, &conn, screen_num, p);
        assert_eq!(r, ans);
    }
}

fn warp_abs(x: i32, y: i32, conn: &Connection, screen_num: i32) {
    let setup = conn.get_setup();
    let screen = setup.roots().nth(screen_num as usize).expect("screen err");
    let root_id = screen.root();
    xcb::warp_pointer(
        conn, XCB_NONE, root_id, 0 as i16, 0 as i16, 0 as u16, 0 as u16, x as i16, y as i16,
    )
    .request_check()
    .expect("Warp pointer failed?"); // Can we do unchecked?
}

fn get_pointer_coords_now(conn: &Connection, screen_num: i32) -> Point {
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

    // Lent from print_focussed_selected.c example
    if unsafe { (*event).source.is_null() } {
        return;
    }
    // Want to look in here later - Accident prevention
    if unsafe { (*(*event).source).states.is_null() } {
        return;
    }

    // Only the caret-moved events from NON-read-only text accessible objects are relevant to tow.
    // It seems 'EDITABLE' rules out terminals?
    if unsafe {
        atspi_state_set_contains(
            (*(*event).source).states,
            // AtspiStateType_ATSPI_STATE_EDITABLE,
            AtspiStateType_ATSPI_STATE_READ_ONLY,
        ) == gtypes::primitive::TRUE
    } {
        return;
    }

    // The pointer was a borrow &mut, when the listener took it.
    // We cannot eat a lent cake, so we cannot dereference the pointer.

    let pdata = voidptr_data as *mut (CaretTowState, xcb::base::Connection, i32);

    let atspi_text_iface = unsafe { atspi_accessible_get_text_iface((*event).source) };
    let atspi_caret_offset = unsafe { atspi_text_get_caret_offset(atspi_text_iface, null_mut()) };
    let atspi_id: i32 = unsafe { atspi_accessible_get_id((*event).source, null_mut()) } as i32;

    // === Surrogate caret position:
    // Caret coordinates are not available, however
    // the bounding box of the glyph at the caret offset is available.
    //
    let glyph_extents = unsafe {
        atspi_text_get_character_extents(
            atspi_text_iface,
            atspi_caret_offset,
            ATSPI_COORD_TYPE_SCREEN,
            null_mut(), // GError
        )
    };

    let caret_coords_now = unsafe { Point((*glyph_extents).x as i32, (*glyph_extents).y as i32) };
    if caret_coords_now == Point(0, 0) {
        return;
    }

    // Rust assures we'll never consume the lent cake, however we are allowed to destructure it!
    let (state, conn, screen_num) = unsafe { pdata.as_mut().expect("NULL pointer passed!") };
    let pointer_coords_now: Point = get_pointer_coords_now(conn, *screen_num);

    let periodically = |dur, state: &mut CaretTowState| {
        if state.mvset == false {
            state.mvset = true;
            state.accessible_id = Some(atspi_id);
            state.glyph_coords_begin = Some(caret_coords_now);
            state.pointer_at_begin = Some(pointer_coords_now);
            state.timestamp = Some(Instant::now());
        }

        // During acquisition of caret events,
        // the origin of the events needs to be the same
        // Otherwise we see unwanted slides.

        if state.accessible_id.unwrap() != atspi_id {
            state.mvset = false;
            state.reset();
            return;
        }

        // Tow needs to learn to discern synthetic moves device moves.
        // Hopefully through xcb events.
        // until then the cure is worse than the disease
        // thus disable the check below
        //
        /*         if state.pointer_at_begin.unwrap() != pointer_coords_now {
            state.mvset = false;
            state.reset();
            return;
        } */

        if state.timestamp.unwrap().elapsed() >= dur {
            do_tow(
                caret_coords_now.0 - state.glyph_coords_begin.unwrap().0,
                caret_coords_now.1 - state.glyph_coords_begin.unwrap().1,
                conn,
                *screen_num,
                pointer_coords_now,
            );

            state.reset();
            return;
        }
    };

    // 'Move per X events is fine, until:
    // - somewhere between 0..X the pointer is moved
    // and with it the view port.

    let glyphcnt = |nc, state: &mut CaretTowState| {
        if state.mvset == false {
            state.mvset = true;
            state.accessible_id = Some(atspi_id);
            state.glyph_coords_begin = Some(caret_coords_now);
            state.pointer_at_begin = Some(pointer_coords_now);
            state.counter = 0;
        }

        // During acquisition of caret events,
        // the origin of the events needs to be the same
        // Otherwise we see unwanted slides.

        if state.accessible_id.unwrap() != atspi_id {
            state.mvset = false;
            state.reset();
            return;
        }
        // Tow needs to learn to discern synthetic moves device moves.
        // Hopefully through xcb events.
        // until then the cure is worse than the disease
        // thus disable the check below
        //
        /*         if state.pointer_at_begin.expect("state: no begin") != pointer_coords_now {
            state.mvset = false;
            state.reset();
            return;
        } */

        if state.counter == 0 {
            state.counter = 1;
            state.pointer_caret_offset();
            return;
        } else if state.counter > 0 && state.counter < nc {
            state.counter += 1;
            return;
        } else if state.counter >= nc {
            do_tow(
                caret_coords_now.0 - state.glyph_coords_begin.unwrap().0,
                caret_coords_now.1 - state.glyph_coords_begin.unwrap().1,
                conn,
                *screen_num,
                Point(
                    state.glyph_coords_begin.unwrap().0 + state.pointer_caret_offset.0,
                    state.glyph_coords_begin.unwrap().1 + state.pointer_caret_offset.1,
                ),
            );
            state.reset();
            return;
        }
    };

    let each_character = |state: &mut CaretTowState| {
        if state.mvset == false {
            state.pointer_at_begin = Some(pointer_coords_now);
            state.glyph_coords_begin = Some(caret_coords_now);
            state.pointer_caret_offset();
            state.mvset = true;
            return;
        }
        // === FIXME need to see wether user moved pointer.
        // Then adjust for that case, probably mvset = false is enough

        let x = caret_coords_now.0 + state.pointer_caret_offset.0;
        let y = caret_coords_now.1 + state.pointer_caret_offset.1;
        warp_abs(x, y, conn, *screen_num);
    };

    match state.behavior {
        Behavior::Interval { dur } => periodically(dur, state),
        Behavior::PerQty { nc } => glyphcnt(nc as u32, state),
        Behavior::Typewriter => each_character(state),
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
    // tossed around state
    let mut cts: CaretTowState = CaretTowState {
        accessible_id: None,
        counter: 0,
        mvset: false,
        timestamp: None,
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
        .arg(Arg::with_name("behavior")
            .short("b")
            .long("behavior")
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
                        cts.behavior = Behavior::PerQty { nc: 100 as u32 };
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
    }

    let (conn, screen_num) = xcb::Connection::connect(None).expect("Failed xcb connection.");
    cts.pointer_at_begin = Some(get_pointer_coords_now(&conn, screen_num));

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
