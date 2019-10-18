#![warn(clippy::all)]
#![allow(non_camel_case_types, dead_code)]

use crate::point::Point;
use crossbeam::atomic::AtomicCell;
use std::time::Duration;

#[derive(Debug)]
pub(crate) struct CaretTowState {
    pub(crate) accessible_id: AtomicCell<Option<i32>>,
    pub(crate) prev_moved_to: AtomicCell<Option<Point>>,
    pub(crate) move_flag: AtomicCell<bool>,
    pub(crate) pointer_caret_offset: AtomicCell<(i32, i32)>, // an offset is semantically not a point
    pub(crate) glyph_coords_begin: AtomicCell<Option<Point>>,
    pub(crate) focus_found_glyph: AtomicCell<Option<Point>>,
    pub(crate) pointer_coords_now: AtomicCell<Option<Point>>,
    pub(crate) caret_coords_now: AtomicCell<Option<Point>>,
    pub(crate) behavior: AtomicCell<Behavior>,
}
impl CaretTowState {
    pub(crate) fn get_accessible_id(&self) -> Option<i32> {
        self.accessible_id.load()
    }
    pub(crate) fn set_accessible_id(&self, val: Option<i32>) {
        self.accessible_id.store(val);
    }
    pub(crate) fn get_prev_moved_to(&self) -> Option<Point> {
        self.prev_moved_to.load()
    }
    pub(crate) fn set_prev_moved_to(&self, val: Option<Point>) {
        self.prev_moved_to.store(val);
    }
    pub(crate) fn move_flag(&self) -> bool {
        self.move_flag.load()
    }
    pub(crate) fn set_move_flag(&self, val: bool) {
        self.move_flag.store(val);
    }
    pub(crate) fn get_pointer_caret_offset(&self) -> (i32, i32) {
        self.pointer_caret_offset.load()
    }
    pub(crate) fn set_pointer_caret_offset(&self, val: (i32, i32)) {
        self.pointer_caret_offset.store(val);
    }

    pub(crate) fn get_glyph_coords_begin(&self) -> Option<Point> {
        self.glyph_coords_begin.load()
    }
    pub(crate) fn set_glyph_coords_begin(&self, val: Option<Point>) {
        self.glyph_coords_begin.store(val);
    }
    pub(crate) fn focus_found_glyph(&self) -> Option<Point> {
        self.focus_found_glyph.load()
    }
    pub(crate) fn set_focus_found_glyph(&self, val: Option<Point>) {
        self.focus_found_glyph.store(val);
    }
    pub(crate) fn get_pointer_coords_now(&self) -> Option<Point> {
        self.pointer_coords_now.load()
    }
    pub(crate) fn set_pointer_coords_now(&self, val: Option<Point>) {
        self.pointer_coords_now.store(val);
    }
    pub(crate) fn get_caret_coords_now(&self) -> Option<Point> {
        self.caret_coords_now.load()
    }
    pub(crate) fn set_caret_coords_now(&self, val: Option<Point>) {
        self.caret_coords_now.store(val);
    }
    pub(crate) fn get_behavior(&self) -> Behavior {
        self.behavior.load()
    }
    pub(crate) fn set_behavior(&self, val: Behavior) {
        self.behavior.store(val);
    }

    pub(crate) fn pointer_caret_offset(&self) {
        self.pointer_caret_offset.store((
            self.pointer_coords_now.load().expect("no pointer begin").0
                - self
                    .glyph_coords_begin
                    .load()
                    .expect("no glyph coords found")
                    .0,
            self.pointer_coords_now.load().expect("no pointer begin").1
                - self
                    .glyph_coords_begin
                    .load()
                    .expect("no glyph coords found")
                    .1,
        ));
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Behavior {
    Pulse { dur: Duration },
    Typewriter,
}
