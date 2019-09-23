

    let periodically = |dur, state: &mut CaretTowState| {
        if !state.mvset {
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
        if !state.mvset {
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
        } else if state.counter > 0 && state.counter < nc {
            state.counter += 1;
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
        if !state.mvset {
            state.pointer_at_begin = Some(pointer_coords_now);
            state.glyph_coords_begin = Some(caret_coords_now);
            state.pointer_caret_offset();
            state.mvset = true;
            return;
        }
        // === FIXME need to see wether user moved pointer.
        // Then adjust for that case, probably mvset = false is enough

        state.pointer_at_begin = Some(pointer_coords_now);
        state.glyph_coords_begin = Some(caret_coords_now);
        state.pointer_caret_offset();

        let x = caret_coords_now.0 + state.pointer_caret_offset.0;
        let y = caret_coords_now.1 + state.pointer_caret_offset.1;
        warp_abs(x, y, conn, *screen_num);
    };