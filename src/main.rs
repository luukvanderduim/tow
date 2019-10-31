/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
* file, You can obtain one at https://mozilla.org/MPL/2.0/. */

#![warn(clippy::all)]
#![allow(non_camel_case_types)]

/// Tow
///
/// An convenience helper program for desktop zoom users.
/// Tow lets the zoom area be towed by the keyboard caret.
/// It is made with the Xfwm4 desktop zoom in mind,
/// but it is not purposely restricted to it.
/// It might work in other desktop environments aswell.
use clap::{crate_version, App, Arg};
use log::{debug, info, warn};
use simple_logger;

use daemonize::Daemonize;

use glib::translate::from_glib_full;
use glib_sys::{GDestroyNotify, GError};
use gtypes::gpointer;

use std::{f64::consts::E, ffi::CString, fs::File, sync::Arc, time::Duration};

use crossbeam::atomic::AtomicCell;
use crossbeam::channel::{Receiver, Sender};
use crossbeam_utils::sync::{Parker, Unparker};

mod state;
use state::{Behavior, CaretTowState};
mod point;
use point::Point;

use xcb;
use xcb::base::Connection;
use xcb::ffi::base::XCB_NONE;

use atspi::{
    Accessible, AccessibleExt, CoordType, Event, StateSet, StateSetExt, StateType, TextExt,
};

use atspi_sys::{
    atspi_event_listener_new, atspi_event_listener_register, atspi_exit, atspi_init, AtspiEvent,
    AtspiEventListenerCB,
};

const SLIDE_DUR: Duration = Duration::from_millis(866);
const FRAME_CALC: u64 = (1000.0 / 60.0) as u64;
const FRAME_DUR: Duration = Duration::from_millis(FRAME_CALC);
const BIG: f64 = 1000.0;

type Move = (Point, i32, i32);

/* fn get_caret_coords_in_focussed_accessible() -> Result<Option<Point>, Error> {
    let mut ret: Result<Option<Point>, Error> = Ok(None);
    for n in 0..atspi::get_desktop_count() {
        println!("Desktop number: {} ", &n);
        match atspi::other::get_desktop(n) {
            Some(desktop) => {
                let cnt: i32 = desktop.get_child_count()?;
                println!("{} number of children", &cnt);
                for i in 0..cnt {
                    let app: Accessible = desktop.get_child_at_index(i)?;
                    println!("child at index {}, accessible handle", &i);

                    if let Some(sset) = app.get_state_set() {
                        if sset.contains(StateType::Focused) {
                            if let Some(ti) = app.get_text_iface() {
                                println!("Focused child has text interface");
                                let o: i32 = ti.get_caret_offset()?;
                                let ext: Rect = ti.get_character_extents(o, CoordType::Screen)?;

                                return Ok(Some(Point(ext.get_x(), ext.get_y())));
                            }
                        }
                    } //  SteteSet is None
                } // for loop # in desktop #
            }

            _ => {
                ret = Ok(None);
            }
        }; // end of match
    } // for loop over
    println!("No more desktops to go through");
    ret
} */

//  .map(|x| (E.powf(x) - E.powf(-x)) / (E.powf(x) + E.powf(-x))) // tanh

fn get_sigmoid_shape() -> Vec<f64> {
    // Slide in ms / Frame in ms = frames / slide
    let frames_per_slide = ((SLIDE_DUR.as_secs() * 1000_u64 + u64::from(SLIDE_DUR.subsec_millis()))
        / (FRAME_DUR.as_secs() * 1000_u64 + u64::from(FRAME_DUR.subsec_millis())))
        as f64;

    let dx: f64 = 1.0 / (frames_per_slide / 2.0);
    let sigma: Vec<f64> = (0..1000)
        .step_by((dx * 1000f64) as usize) // Lossy cast
        .map(|x| f64::from(x) / 1000f64)
        .map(|x| 1.0f64 / (1.0f64 + E.powf(12.0 * x - 6.0)))
        .collect();

    let innate_sum = sigma.iter().sum::<f64>();

    // FIRST multiply by BIG number ONLY THEN normalize
    //
    let mut sigm: Vec<f64> = sigma
        .into_iter()
        .map(|x| x * BIG)
        .map(|x| x / innate_sum)
        .map(|x| x / 2.0)
        .collect();

    // shape yields a _/\_ shaped curve
    // shape contains two sigmoids of which the first is reversed.

    let mut shape: Vec<f64> = sigm.clone().into_iter().rev().collect(); //clone needed!
    shape.append(&mut sigm);

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

fn do_tow(mut begin: Point, dx: i32, dy: i32, co: Arc<Connection>, screen_num: i32) -> Point {
    let shapevalues: Vec<f64> = get_sigmoid_shape();
    let len = shapevalues.len();
    let threshold = BIG / len as f64;

    // xmove in amount of whole pixels to move
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
        xmove_pixels = v * f64::from(dx);
        ymove_pixels = v * f64::from(dy);

        //
        //  X:
        //

        if xmove_pixels.abs() >= threshold {
            //  x moves this frame by:
            x = f64::from(begin.0) + (xmove_pixels + cx) / BIG;

            //  The fraction is carried to the next iteration
            cx = x.fract() * BIG;

            //  Next iterations x starting point
            begin.0 = x.trunc() as i32;
        } else {
            //  Smaller than the threshold, carry all
            cx += xmove_pixels;

            //  Nevertheless set y for the move
            x = f64::from(begin.0);
        }

        // Bonus squeeze
        // last iteration left us with a remainder, we need to process this
        if i == len - 1 {
            x = f64::from(begin.0) + (cx / BIG).round() as f64;
            begin.0 = x as i32;
        }

        //
        //  Y:
        //

        if ymove_pixels.abs() >= threshold {
            //  y moves this frame by:
            y = f64::from(begin.1) + ((ymove_pixels + cy) / BIG) as f64;
            //  The fraction is carried to the next iteration
            cy = y.fract() * BIG;

            //  Next iterations x starting point
            begin.1 = y.trunc() as i32;
        } else {
            //  Smaller than the threshold, carry all
            cy += ymove_pixels;

            //  Nevertheless set y for the move
            y = f64::from(begin.1);
        }

        // === Bonus squeeze
        // last iteration left us with a remainder, we need to process this
        if i == len - 1 {
            y = f64::from(begin.1) + (cy / BIG).round() as f64;
            begin.1 = y as i32;
        }

        // === Crumb tasting!
        // Did last iteration leave a crumb?
        // Taste it to evaluate if we are about to do the same
        // To avoid calling xcb without a cause!
        if let Some(cr) = crumb {
            let co = Arc::clone(&co);
            if cr == (x, y) {
                std::thread::sleep(FRAME_DUR);
            } else {
                warp_abs(x as i32, y as i32, co, screen_num);
                std::thread::sleep(FRAME_DUR);
                crumb = Some((x, y));
            }
        } else {
            let co = Arc::clone(&co);
            warp_abs(x as i32, y as i32, co, screen_num);
            std::thread::sleep(FRAME_DUR);
            crumb = Some((x, y));
        }
    } // end delimiter of for loop
    begin
}
/*
#[cfg(test)]
#[test]
fn test_do_tow() {
    use crate::point::Point;
    use rand::{thread_rng, Rng};

    let mut rng = thread_rng();

    let (conn, screen_num) = xcb::Connection::connect(None).expect("Failed xcb connection.");
    let conn = Arc::new(conn);
    for _ in 0..10 {
        let p: Point = Point(rng.gen_range(-1001, 1001), rng.gen_range(-1001, 1001));
        let q: Point = Point(rng.gen_range(0, 1921), rng.gen_range(0, 1080));
        let ans = p + q;
        let r: Point = do_tow(p, q.0, q.1, conn.clone(), screen_num);
        assert_eq!(r, ans);
    }
} */

fn warp_abs(x: i32, y: i32, co: Arc<Connection>, screen_num: i32) {
    let screen = co
        .get_setup()
        .roots()
        .nth(screen_num as usize)
        .expect("screen err");
    let root_id = screen.root();
    xcb::warp_pointer(
        &co, XCB_NONE, root_id, 0 as i16, 0 as i16, 0 as u16, 0 as u16, x as i16, y as i16,
    )
    .request_check()
    .expect("Warp pointer failed?"); // Can we do unchecked?
}

fn obtain_pointer_coords_now(co: Arc<Connection>, screen_num: i32) -> Option<Point> {
    let screen = co
        .get_setup()
        .roots()
        .nth(screen_num as usize)
        .expect("Cannot get screen!");
    let root_id = screen.root();

    let pointercookie = xcb::xproto::query_pointer(&co, root_id);

    match pointercookie.get_reply() {
        Ok(r) => Some(Point(i32::from(r.root_x()), i32::from(r.root_y()))),
        Err(e) => {
            eprintln!("could not get pointer coordinates: {}", e);
            None
        }
    }
}

fn tow(rx: Receiver<Move>, up: &Unparker, q: Parker, co: Arc<Connection>, screen_num: i32) {
    loop {
        let var = rx.clone().into_iter();
        for mv in var {
            let begin = mv.0;
            let dx = mv.1;
            let dy = mv.2;
            do_tow(begin, dx, dy, co.clone(), screen_num);
        }
        up.unpark();
        q.park();
    }
}

fn pulse_thread(
    state: Arc<CaretTowState>,
    conn: Arc<Connection>,
    screen_num: i32,
    dur: Duration,
    uq: &Unparker,
    p: Parker,
    tx: Sender<Move>,
) {
    loop {
        // In this loop we ONLY act on state
        // STATE CHANGE is caused by events
        //

        p.park_timeout(dur.to_owned());
        let now = obtain_pointer_coords_now(conn.clone(), screen_num).unwrap();
        if state.move_flag() {
            match state.get_prev_moved_to() {
                Some(then) if then != now => {
                    // USER MOVED POINTER
                    // refrain from move (postion is up to date)

                    state.set_prev_moved_to(state.get_pointer_coords_now());
                    state.set_glyph_coords_begin(state.get_caret_coords_now());
                }

                Some(then) if then == now => {
                    // The executor loop thread was parked for dur: Duration
                    // The pointer did not move. Fine.

                    // do_tow() requires:
                    // the current pointer position
                    // and the amount of x and y to move:

                    if let Some(caret_now) = state.get_caret_coords_now() {
                        if let Some(caret_begin) = state.get_glyph_coords_begin() {
                            // If the caret did not move, there is nothing to do.
                            if caret_begin == caret_now {
                                continue;
                            }

                            let (deltax, deltay) =
                                { (caret_now.0 - caret_begin.0, caret_now.1 - caret_begin.1) };
                            let aim = now + Point(deltax, deltay);

                            match tx.send((now, deltax, deltay)) {
                                Ok(()) => {
                                    state.set_prev_moved_to(Some(aim));
                                    state.set_glyph_coords_begin(state.get_caret_coords_now());
                                    uq.unpark();
                                }
                                Err(e) => {
                                    eprintln!("Cannot send move to tow {:?}", e);
                                }
                            }
                        }
                    }
                }
                None => {
                    if let Some(caret_now) = state.get_caret_coords_now() {
                        if let Some(caret_begin) = state.get_glyph_coords_begin() {
                            let (deltax, deltay) =
                                { (caret_now.0 - caret_begin.0, caret_now.1 - caret_begin.1) };

                            // If the caret did not move, there is nothing to do.
                            if caret_now == caret_begin {
                                continue;
                            }

                            let aim = now + Point(deltax, deltay);

                            if cfg!(debug_assertions) && aim == Point(0, 0) {
                                println!("aimed for (0,0)");
                            }

                            match tx.send((now, deltax, deltay)) {
                                Ok(()) => {
                                    state.set_prev_moved_to(Some(aim));
                                    state.set_glyph_coords_begin(state.get_caret_coords_now());
                                    uq.unpark();
                                }
                                Err(e) => {
                                    eprintln!("Cannot send move to tow {:?}", e);
                                }
                            }
                        }
                    }
                }
                _ => {
                    dbg!("We are not supposed to get here?");
                }
            }
        } else {
            // move_flag false
            if let Some(caret_now) = state.get_caret_coords_now() {
                if let Some(caret_begin) = state.get_glyph_coords_begin() {
                    // If the caret did not move, there is nothing to do.
                    if caret_begin == caret_now {
                        continue;
                    }
                    let (offset_x, offset_y) = state.get_pointer_caret_offset();
                    let aim = now + Point(offset_x, offset_y);
                    match tx.send((caret_now, offset_x, offset_y)) {
                        Ok(()) => {
                            state.set_prev_moved_to(Some(aim));
                            state.set_glyph_coords_begin(state.get_caret_coords_now());
                            uq.unpark();
                        }
                        Err(e) => {
                            eprintln!("Cannot send move to tow {:?}", e);
                        }
                    }
                }
            }
        }
        std::thread::yield_now();
    } // belongs to loop
}

extern "C" fn brooming(data: gpointer) {
    unsafe { glib_sys::g_free(data) };
}

#[no_mangle]
extern "C" fn on_focus_changed(event: *mut AtspiEvent, voidptr_data: *mut ::std::ffi::c_void) {
    let ev_source: Accessible = unsafe { from_glib_full((*event).source) };
    let ev_detail1 = unsafe { (*event).detail1 } as i32;

    let pdata = voidptr_data as *mut (Arc<CaretTowState>, Arc<Connection>, i32);
    let (state, conn, screen_num) = unsafe { pdata.as_mut().expect("Wonderous machine error!") };

    // Lets check if the fucus was changed by an Accessible source with a editable
    // text interface. eg. not a pop-up message or a progress bar message or some other
    // message widget we are not about to edit.
    let acc_stateset: StateSet = ev_source
        .get_state_set()
        .expect("Unable to get state from accessible thet emited event.");
    if acc_stateset.contains(StateType::ReadOnly) {
        return;
    }

    // In focus ev
    //
    // If its the first accessible in a move to get focus, set ID
    // (atspi_id was None )
    //  - set glyph_begin and caret now to found coordinates
    //
    //
    // If the user caused the focus ev:
    //  that is: if prev moved to != current
    // - update id
    // - set glyph_begin and caret now to found coordinates
    // - set pointer_caret_offset
    //
    //
    // If the user did not cause the focus ev
    // == Choice == move or not to move
    // - change id
    // - induce move
    let current_pointer = obtain_pointer_coords_now(conn.clone(), screen_num.to_owned()).unwrap();

    let update_glyph_and_caret = || {
        // On focus change we want to set the caret coordinates in the global state
        // because this is likely our first opportunity
        if let Some(atspi_text_iface) = ev_source.get_text_iface() {
            match atspi_text_iface.get_character_extents(ev_detail1, CoordType::Screen) {
                Err(e) => {
                    println!("No rect on caret offset, {:?}", &e);
                }
                Ok(glyph_extents) => {
                    state.set_caret_coords_now(Some(Point(
                        glyph_extents.get_x(),
                        glyph_extents.get_y(),
                    )));
                    state.set_glyph_coords_begin(Some(Point(
                        glyph_extents.get_x(),
                        glyph_extents.get_y(),
                    )));
                }
            }
        }
    };

    let ev_id: Option<i32> = None;
    if state.get_accessible_id().is_none() {
        match ev_source.get_id() {
            Ok(ev_id) => {
                state.set_accessible_id(Some(ev_id));
                update_glyph_and_caret();

                state.pointer_caret_offset();
                return;
            }
            Err(e) => {
                eprintln!("Error geting accessibles id: {:?}", e);
            }
        }
    }

    if let Some(prev) = state.get_prev_moved_to() {
        if current_pointer != prev {
            // user caused focus change by moving (mouse) pointer
            // to other editable text of interest
            update_glyph_and_caret();
            state.set_pointer_coords_now(Some(current_pointer));
            state.pointer_caret_offset();
            state.set_accessible_id(ev_id);
        } else {
            // Some other event caused focus change
            // Popup with editable text, we want to go there
            // We may want to go there with sane defaults and set  move
            state.set_accessible_id(ev_id);
            state.set_move_flag(false); // induce move
            return;
        }
    }
}

#[no_mangle]
extern "C" fn on_caret_move(event: *mut AtspiEvent, voidptr_data: *mut ::std::ffi::c_void) {
    //
    //  Event {
    //      type: class:major:minor eg. 'object:caret-cursor-changed;
    //      source: Accessible  belonging to the application that caused the event
    //      detail1: deoends on the type.[1]
    //      detail2: deoends on the type
    //      user_data: depends on the type
    //      sender: Accessible (>=v2.34) equals source except when the event is caused by the A11y client
    //  }
    //
    //  [1] <https://accessibility.linuxfoundation.org/a11yspecs/atspi/adoc/atspi-events.html>
    //
    // get the accessible that caused this event
    // and get the state set associated with this accessible

    let ev_source: Accessible = unsafe { from_glib_full((*event).source) };
    let ev_detail1 = unsafe { (*event).detail1 } as i32;

    let acc_stateset: StateSet = ev_source
        .get_state_set()
        .expect("Unable to get state from accessible thet emitted event.");

    // Only the caret-moved events from Not-read-only text accessible objects are relevant to tow.
    // It seems 'Editable' rules out terminals?
    if acc_stateset.contains(StateType::ReadOnly) {
        return;
    }

    // The pointer was a borrowed &mut, when the listener took it.
    // We cannot eat a lent cake, so we cannot dereference the pointer.

    let pdata = voidptr_data as *mut (Arc<CaretTowState>, Arc<Connection>, i32);
    let (state_f, conn_f, screen_num) = unsafe { pdata.as_mut().unwrap() };

    let state: Arc<CaretTowState> = Arc::clone(state_f);
    let conn: Arc<Connection> = Arc::clone(conn_f);

    // === Surrogate caret position:
    // Caret coordinates are not available, however
    // the bounding box of the glyph at the caret offset is available.
    // that will do just fine:
    //
    if let Some(atspi_text_iface) = ev_source.get_text_iface() {
        match atspi_text_iface.get_character_extents(ev_detail1, CoordType::Screen) {
            Err(e) => {
                println!("No rect on caret offset, {:?}", &e);
                return;
            }
            Ok(glyph_extents) => {
                state.set_caret_coords_now(Some(Point(
                    glyph_extents.get_x(),
                    glyph_extents.get_y(),
                )));
            }
        }
    }

    match ev_source.get_id() {
        Ok(atspi_id) => {
            if let Some(id) = state.get_accessible_id() {
                if id != atspi_id {
                    // found id but of different application
                    // this should not be
                    warn!("different application steals focus");
                    warn!(
                        "focus is claimed by {:?}",
                        ev_source.get_name().unwrap().as_str()
                    );
                }
            } else if state.get_accessible_id().is_none() {
                state.set_accessible_id(Some(atspi_id));
            }
        }
        Err(e) => {
            eprintln!("Caret move but no accessbie id, {:?}", e);
            return;
        }
    };
    let atspi_id = state.get_accessible_id().unwrap();
    // During acquisition of caret events,
    // the origin of the events needs to be the same

    state.set_pointer_coords_now(obtain_pointer_coords_now(
        conn.clone(),
        screen_num.to_owned(),
    ));

    if state.move_flag() && state.get_accessible_id().unwrap() != atspi_id {
        state.set_glyph_coords_begin(state.get_caret_coords_now());
        state.pointer_caret_offset();
        state.set_move_flag(false);
        state.set_accessible_id(Some(atspi_id));
        return;
    }

    state.set_accessible_id(Some(atspi_id));

    let conn = Arc::clone(&conn);
    let pulse = |_dur| {
        // caret coords now is set
        // pointer coords now is set
        // accessible_id is set

        if !state.move_flag() {
            state.set_move_flag(true);

            // Glyph coords information might have been found in a focus event
            // prior to this event
            // if so, we set focus_found_glyph as the begin

            if let Some(focus_glyph) = state.focus_found_glyph() {
                state.set_glyph_coords_begin(Some(focus_glyph));
                state.set_focus_found_glyph(None);
            } else {
                state.set_glyph_coords_begin(state.get_caret_coords_now());
            }
            state.pointer_caret_offset();
        }
    };

    let each_glyph = || {
        if !state.move_flag() {
            if state.get_glyph_coords_begin().is_none() {
                state.set_glyph_coords_begin(state.get_caret_coords_now());
            }
            // set variable 'pointer_caret_offset' in global state
            // Difference between pointer and glyph at beginposition
            // We use this difference to keep the relative distance
            // between caret and pointer the same.
            // Caveat: no longer works when the mouse is moved
            state.pointer_caret_offset();
            state.set_move_flag(true);

            let x = state.get_caret_coords_now().unwrap().0 + state.get_pointer_caret_offset().0;
            let y = state.get_caret_coords_now().unwrap().1 + state.get_pointer_caret_offset().1;

            let screen = screen_num.to_owned();
            warp_abs(x, y, conn.clone(), screen);
            state.set_prev_moved_to(Some(Point(x, y)));
            state.set_glyph_coords_begin(Some(Point(x, y)));
            return;
        }

        match state.get_prev_moved_to() {
            Some(expected) if expected != state.get_pointer_coords_now().unwrap() => {
                // The pointer has been moved by user
                // skip move (postion is up to date)
                // set new pointer and caret begins in state
                // have new pointer_caret_offset calculated
                // wait for new event
                state.set_glyph_coords_begin(state.get_caret_coords_now());
                state.pointer_caret_offset();
                state.set_move_flag(false);
            }

            Some(expected) if expected == state.get_pointer_coords_now().unwrap() => {
                // The 'normal'case
                let x =
                    state.get_caret_coords_now().unwrap().0 + state.get_pointer_caret_offset().0;
                let y =
                    state.get_caret_coords_now().unwrap().1 + state.get_pointer_caret_offset().1;

                let screen = screen_num.to_owned();

                warp_abs(x, y, conn.clone(), screen);
                state.set_prev_moved_to(Some(Point(x, y)));
            }
            _ => {}
        }
    };

    match state.get_behavior() {
        Behavior::Pulse { dur } => {
            pulse(dur);
        }
        Behavior::Typewriter => {
            each_glyph();
        }
    }
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
    simple_logger::init().unwrap_or_else(|e| eprintln!("Log initialization failed: {:?}", e));
    info!("Captains log opened.");

    let (conn, screen_num) = {
        let (conn, screen_num) =
            xcb::Connection::connect(None).expect("Cannot obtain X Connection in main!");
        (Arc::new(conn), screen_num)
    };

    // mutual state
    let cts: Arc<CaretTowState> = Arc::new(CaretTowState {
        accessible_id: AtomicCell::new(None),
        prev_moved_to: AtomicCell::new(None),
        move_flag: AtomicCell::new(false),
        pointer_caret_offset: AtomicCell::new((0, 0)),
        glyph_coords_begin: AtomicCell::new(None),
        focus_found_glyph: AtomicCell::new(None),
        pointer_coords_now: AtomicCell::new(None),
        caret_coords_now: AtomicCell::new(None),
        behavior: AtomicCell::new(Behavior::Pulse {
            dur: Duration::from_millis(2000),
        }),
    });

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
            Arg::with_name("behavior")
                .short("b")
                .long("behavior")
                .takes_value(true)
                .help("Mode: pulse [N: 100-10000] (ms) or typewriter (default)")
                .max_values(2),
        )
        .get_matches();

    if matches.is_present("daemon") {
        spookify_tow();
    }

    if matches.is_present("behavior") {
        let mut bvals = matches.values_of("behavior").expect("Unexpected!");
        match bvals.next() {
            Some("pulse") => {
                if let Some(numb) = bvals.next() {
                    let n: u16 = numb.parse::<u16>().expect("u16 parse error");
                    if n <= 99 {
                        cts.set_behavior(Behavior::Pulse {
                            dur: Duration::from_millis(100),
                        });
                    }
                    if n >= 100 && n <= 10000 {
                        cts.set_behavior(Behavior::Pulse {
                            dur: Duration::from_millis(u64::from(n)),
                        });
                    } else {
                        cts.set_behavior(Behavior::Pulse {
                            dur: Duration::from_millis(2000),
                        });
                    }
                }
            }
            Some("typewriter") => {
                cts.set_behavior(Behavior::Typewriter);
            }
            Some(&_) => {
                eprintln!("Error: Invalid 'behavior' value. Typo?");
            }
            None => {
                eprintln!("Error: Invalid 'behavior'  value.");
            }
        }
    }

    cts.set_pointer_coords_now(obtain_pointer_coords_now(conn.clone(), screen_num));

    // AT-SPI init
    if unsafe { atspi_init() } != 0 {
        panic!("Could not initialise AT-SPI.");
    }

    /*     match get_caret_coords_in_focussed_accessible() {
        Ok(caret_at_start) => {
            match caret_at_start {
                Some(pos) => {
                    println!("caret at start: Point({},{})", pos.0, pos.1);
                }
                None => {
                    println!("No postition");
                }
            }
            cts.glyph_coords_begin = caret_at_start;
        }
        Err(error) => {
            eprintln!("{:?}", error);
        }
    } */

    #[no_mangle]
    let mut triplet = (cts.clone(), conn.clone(), screen_num);
    #[no_mangle]
    let voidptr_data: *mut std::ffi::c_void =
        &mut triplet as *mut (Arc<CaretTowState>, Arc<Connection>, i32) as *mut std::ffi::c_void;

    #[no_mangle]
    let evfn: AtspiEventListenerCB = Some(on_caret_move);
    #[no_mangle]
    let evfn2: AtspiEventListenerCB = Some(on_focus_changed);
    let post_event_chores: GDestroyNotify = Some(brooming);

    // AT-SPI event listener
    let caret_listener = unsafe { atspi_event_listener_new(evfn, voidptr_data, post_event_chores) };
    let focus_listener =
        unsafe { atspi_event_listener_new(evfn2, voidptr_data, post_event_chores) };

    let evtype_caret_moved = CString::new("object:text-caret-moved")
        .expect("CString::new failed")
        .into_raw() as *const i8;

    //let evtype_focus_changed = CString::new("object:state-changed:focused")
    let evtype_focus_changed = CString::new("object:state-changed:focused")
        .expect("CString::new failed")
        .into_raw() as *const i8;

    let err: *mut *mut GError = std::ptr::null_mut();
    unsafe {
        atspi_event_listener_register(caret_listener, evtype_caret_moved, err);
        atspi_event_listener_register(focus_listener, evtype_focus_changed, err);
        if !err.is_null() {
            println!(" Err: {:?}", err);
        }
    }
    crossbeam::thread::scope(|s| {
        let conn = Arc::clone(&conn);
        let cts = Arc::clone(&cts);

        s.builder()
            .name("Tow AT-SPI event thread".to_string())
            .spawn(move |_| {
                Event::main();
            })
            .unwrap();

        if let Behavior::Pulse { dur } = cts.get_behavior() {
            let p = Parker::new();
            let up = p.unparker().clone();
            let q = Parker::new();
            let uq = q.unparker().clone();

            let (tx, rx) = crossbeam::channel::unbounded::<Move>();
            let conn_a = Arc::clone(&conn);
            let conn_b = Arc::clone(&conn);
            let cts_a = Arc::clone(&cts);

            s.builder()
                .name("Captain Pulse".to_string())
                .spawn(move |_| {
                    pulse_thread(
                        cts_a,
                        conn_a.clone(),
                        screen_num.to_owned(),
                        dur,
                        &uq,
                        p,
                        tx,
                    );
                })
                .unwrap();

            s.builder()
                .name("ms Tow - tug thread".to_string())
                .spawn(move |_| {
                    q.park();
                    tow(rx.clone(), &up, q, conn_b.clone(), screen_num.to_owned());
                })
                .unwrap();
        }
    })
    .expect("scope fault");

    if unsafe { atspi_exit() } != 0 {
        eprintln!("AT-SPI exit failed.");
    }

    unsafe {
        gobject_sys::g_object_unref(err as *mut _); // as GError
        glib_sys::g_free(evtype_caret_moved as gpointer);
        glib_sys::g_free(evtype_focus_changed as gpointer);
        gobject_sys::g_object_unref(caret_listener as *mut gobject_sys::GObject);
        gobject_sys::g_object_unref(focus_listener as *mut gobject_sys::GObject);
        gobject_sys::g_object_unref(voidptr_data as *mut gobject_sys::GObject);
    }
}
