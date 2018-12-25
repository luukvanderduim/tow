// tow: an Xfce 'desktop zoom' add-on utility
//
// 'tow' has the zoom area view port be towed by the keyboard caret
//
// A user directs the Xfce zoom area view port (vp) to some (text input) area by means of eg. the mouse device.
// (mouse or any other pointer device of choice).
// Typically both hands are in use when typing, the caret (keyboard cursor) is moving away while
// the zoom area vp stays put until the user decides to move the pointer to match the caret's position.
//
// Tow aims to lessen the amount of manual zoom area adjustment by having it be towed by the caret.

// ============================================================
//                        !! Caution !!
// ============================================================
// It is likely we break something:
// X11::XInput2 / libxdo are not intended for repetitive calls.
//
// This program is to be regarded as EXPERIMENTAL

#![feature(duration_as_u128)]
#![allow(unused_imports)]

use kahan::{KahanSum, KahanSummator};
use libxdo::XDo;
use std::f64::consts::E;
use std::thread;
use std::time::{Duration, Instant};

// SLIDE_DUR defines the time it takes to smoothly animate
// the zoom area being 'towed to' the keyboard caret (keyboard cursor).

const SLIDE_DUR: Duration = Duration::from_micros(1_000_000);
const FRAME_DUR: Duration = Duration::from_micros(1_000_000 / 30);

/* enum move_style {
    sigmoid,
    warp,
    typewriter,
    brachistochrone,
} */

// static ms: move_style = sigmoid;

// 'Sigmoid' (S-shaped curve). Tow uses tanh in it's 0..2 range to model
// smooth slopes.
// sigmoid64(): tanh curve over range 0..2, then normalized

fn get_sigmoid() -> Vec<f64> {
    let xd: f64 = 2.0 / ((SLIDE_DUR.as_micros() / FRAME_DUR.as_micros()) / 2) as f64;
    let sigmoid: Vec<f64> = (0..2_000_000)
        .step_by((xd * f64::from(1_000_000)) as usize)
        .map(|x| f64::from(x) / f64::from(1_000_000))
        .map(|x| (E.powf(x) - E.powf(-x)) / (E.powf(x) + E.powf(-x)))
        .collect();

    let ksum: KahanSum<f64> = sigmoid.clone().into_iter().kahan_sum();
    let normalized_sigmoid: Vec<f64> = sigmoid
        .clone()
        .into_iter()
        .map(|x| x / ksum.sum())
        .collect();

    normalized_sigmoid
}

fn do_move(xdo: XDo, mut qu: Vec<f64>, mut dx: u64, c: f64) {
    if qu.is_empty() {
        println!("qu empty: correct");
        return;
    } else {
        std::thread::sleep(FRAME_DUR);
        let mut moc: f64;

        if let Some(value) = qu.first() {
            moc = value * dx as f64 + c;
            qu.remove(0);
        } else {
            println!("Wonderous machine!!");
            return;
        }

        if moc < 1.0 {
            do_move(xdo, qu, dx, moc)
        } else {
            xdo.move_mouse_relative(moc.trunc() as i32, 0)
                .expect("Failed to move!");
            dx -= moc.trunc() as u64;
            do_move(xdo, qu, dx, moc.fract());
        }
    }
}

fn main() {
    let x1: i32 = 0;
    let x2: i32 = 300;
    let distance: u64 = (x2 - x1).abs() as u64;
    let xdo = XDo::new(None).expect("Failed to grab X11 vua libXDo");
    let mut halfqueue = get_sigmoid().clone();
    let mut queue: Vec<f64> = get_sigmoid().clone();

    while !halfqueue.is_empty() {
        if let Some(v) = halfqueue.pop() {
            queue.push(v);
        } else {
            println!("Wonderous machine!");
        }
    }
    do_move(xdo, queue, distance, 0.0);
}
