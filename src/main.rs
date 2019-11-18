/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
* file, You can obtain one at https://mozilla.org/MPL/2.0/. */

#![warn(clippy::all)]
#![allow(non_camel_case_types)]

/// Tow
///
/// A zoom convenience helper program for desktop zoom users.
/// Tow lets the zoom area be towed by the keyboard caret.
/// It is made with the Xfwm4 desktop zoom in mind,
/// but it is not purposely restricted to it.
/// It might work in other desktop environments aswell.

use log::{info, warn};
use simple_logger;

use daemonize::Daemonize;




use std::{
    f64::consts::E,
    fs::File,
    sync::Arc,
    time::Duration,
};

use crossbeam::atomic::AtomicCell;
use crossbeam::channel::{Receiver, Sender};
use crossbeam_utils::sync::{Parker, Unparker};

mod arguments;
mod cables;
mod point;
use point::Point;
mod process;
mod state;
use state::{Behavior, CaretTowState};
mod tests;

use xcb;
use xcb::base::Connection;
use xcb::ffi::base::XCB_NONE;
use xcb::{CW_EVENT_MASK, EVENT_MASK_KEY_PRESS, GRAB_MODE_ASYNC, KEY_PRESS, MOD_MASK_ANY};

use atspi::{
    other::{init, exit},
    CoordType, Event, EventListener, EventListenerExt,  
};


const SLIDE_DUR: Duration = Duration::from_millis(866);
const FRAME_CALC: u64 = (1000.0 / 60.0) as u64;
const FRAME_DUR: Duration = Duration::from_millis(FRAME_CALC);
const BIG: f64 = 1000.0;

type Move = (Point, i32, i32);


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
    cts: Arc<CaretTowState>,
    conn: Arc<Connection>,
    screen_num: i32,
    dur: Duration,
    uq: &Unparker,
    p: Parker,
    tx: Sender<Move>,
) {
    loop {
        // we only act on state
        // state change is caused by events

        p.park_timeout(dur.to_owned());
        let now = obtain_pointer_coords_now(conn.clone(), screen_num).unwrap();
        if cts.move_flag() {
            match cts.get_prev_moved_to() {
                Some(then) if then != now => {
                    // USER MOVED POINTER
                    // refrain from move (postion is up to date)

                    cts.set_prev_moved_to(cts.get_pointer_coords_now());
                    cts.set_glyph_coords_begin(cts.get_caret_coords_now());
                }

                Some(then) if then == now => {
                    // The executor loop thread was parked for dur: Duration
                    // The pointer did not move. Fine.

                    // do_tow() requires:
                    // the current pointer position
                    // and the amount of x and y to move:

                    if let Some(caret_now) = cts.get_caret_coords_now() {
                        if let Some(caret_begin) = cts.get_glyph_coords_begin() {
                            // If the caret did not move, there is nothing to do.
                            if caret_begin == caret_now {
                                continue;
                            }

                            let (deltax, deltay) =
                                { (caret_now.0 - caret_begin.0, caret_now.1 - caret_begin.1) };
                            let aim = now + Point(deltax, deltay);

                            match tx.send((now, deltax, deltay)) {
                                Ok(()) => {
                                    cts.set_prev_moved_to(Some(aim));
                                    cts.set_glyph_coords_begin(cts.get_caret_coords_now());
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
                    if let Some(caret_now) = cts.get_caret_coords_now() {
                        if let Some(caret_begin) = cts.get_glyph_coords_begin() {
                            let (deltax, deltay) =
                                (caret_now.0 - caret_begin.0, caret_now.1 - caret_begin.1);

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
                                    cts.set_prev_moved_to(Some(aim));
                                    cts.set_glyph_coords_begin(cts.get_caret_coords_now());
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
            if let Some(caret_now) = cts.get_caret_coords_now() {
                if let Some(caret_begin) = cts.get_glyph_coords_begin() {
                    // If the caret did not move, there is nothing to do.
                    if caret_begin == caret_now {
                        continue;
                    }
                    let (offset_x, offset_y) = cts.get_pointer_caret_offset();
                    let aim = now + Point(offset_x, offset_y);
                    match tx.send((caret_now, offset_x, offset_y)) {
                        Ok(()) => {
                            cts.set_prev_moved_to(Some(aim));
                            cts.set_glyph_coords_begin(cts.get_caret_coords_now());
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
    } // ends loop
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
    crossbeam::thread::scope( |s| {
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
    
    // Best way to handle an argument
    arguments::quarrel(cts.clone());
    
    cts.set_pointer_coords_now(obtain_pointer_coords_now(conn.clone(), screen_num));

    if !init() {
        eprintln!("Failed to init atspi!"); 
    }
    
        s.builder()
            .name("Tow AT-SPI event thread".to_string())
            .spawn( |_| {
                Event::main();
            })
            .unwrap();

        if let Behavior::Pulse { dur } = cts.get_behavior() {
            let p = Parker::new();
            let up = p.unparker().clone();
            let q = Parker::new();
            let uq = q.unparker().clone();

            let (tx, rx) = crossbeam::channel::unbounded::<Move>();
            let setup_conn = Arc::clone(&conn);

            // Jungle of cloned Arcs to satisfy moved items in 
            // possibly longer living closures
            // FIXME: Reduce.  
            let conn_b = Arc::clone(&conn);
            let fev_conn = Arc::clone(&conn);
            let cev_conn = Arc::clone(&conn);
            let cev_cts = Arc::clone(&cts);
            let fev_cts = Arc::clone(&cts);
            let hotk_conn = Arc::clone(&conn);
            s.builder()
                .name("Anchor".to_string())
                .spawn( move |_| {
                    let setup = &setup_conn.get_setup();
                    let sn = screen_num;

                    let caretev_listener: EventListener = EventListener::new( move | event: &Event | process::caret_event_processor(
                        event, cev_cts.clone(), cev_conn.clone(), screen_num  ) );

                    let focusev_listener: EventListener = EventListener::new( move | event: &Event | process::focus_event_processor(
                        event, fev_cts.clone(),  fev_conn.clone(), sn ) );
                      
                   
                    
                    // start / stop with hotkey (F12)
                    // A display may consist of more than one screen, all screens have a root window
                    for screen in setup.roots() {
                        xcb::grab_key_checked(
                            &hotk_conn.clone(),
                            true,
                            screen.root(),
                            MOD_MASK_ANY as u16,
                            96u8,
                            GRAB_MODE_ASYNC as u8,
                            GRAB_MODE_ASYNC as u8,
                        )
                        .request_check()
                        .expect("key grab failed: Maybe another instance of tow is running or the hotkey is already taken");
                        
                        xcb::xproto::change_window_attributes_checked(
                            &hotk_conn,
                            screen.root(),
                            &[(CW_EVENT_MASK, EVENT_MASK_KEY_PRESS)],
                        )
                        .request_check()
                        .expect("Change of window attributes failed");
                    }
                    
                    let mut on: bool = false;
                    loop {
                        &hotk_conn.flush();

                        // Did we receive a hotkey?
                        if let Some(ev) = &hotk_conn.wait_for_event() {
                            if ev.response_type() & !0x80 == KEY_PRESS {
                               
                                if !on {
                                  caretev_listener.register("object:text-caret-moved").expect("Cannot register caret event listener");
                                  focusev_listener.register("object:state-changed:focused").expect("Cannot register focus event listener");
                                    
                                    info!("Tow on");
                                    cables::notify_started();
                                } else {
                                    caretev_listener.deregister("object:text-caret-moved").expect("Cannot deregister caret event listener");
                                    focusev_listener.deregister("object:state-changed:focused").expect("Cannot deregister caret event listener");
                                    
                                    info!("Tow off");
                                    cables::notify_stopped();
                                }
                                on = !on; 
                            }
                        }
                    } // loop ends here
                        
                })
                .unwrap();

            s.builder()
                .name("Pulse".to_string())
                .spawn(move |_| {
                    pulse_thread(
                        cts,
                        conn,
                        screen_num.to_owned(),
                        dur,
                        &uq,
                        p,
                        tx,
                    );
                })
                .unwrap();

            s.builder()
                .name("Tow thread".to_string())
                .spawn(move |_| {
                    q.park();
                    tow(rx.clone(), &up, q, conn_b.clone(), screen_num.to_owned());
                })
                .unwrap();
        }
    })
    .expect("scope fault");

    if !exit() {
        eprintln!("Failed to cleanly exit from libatspi!"); 
    }
}
