use super::*;
use atspi::{Accessible, AccessibleExt, Event, StateSet, StateSetExt, StateType, TextExt};

pub(crate) fn caret_event_processor(
    event: &Event,
    cev_cts: Arc<CaretTowState>,
    cev_conn: Arc<Connection>,
    screen_num: i32,
) {
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

    let ev_source: Accessible = event
        .get_source()
        .expect("No Accessible source member on Event type");
    let ev_detail1 = event.get_detail1();

    let acc_stateset: StateSet = ev_source
        .get_state_set()
        .expect("Unable to get state from accessible thet emitted event.");

    // Only the caret-moved events from Not-read-only text accessible objects are relevant to tow.
    // It seems 'Editable' rules out terminals?
    if acc_stateset.contains(StateType::ReadOnly) {
        return;
    }

    // === Surrogate caret position:
    // Caret coordinates are not available, however
    // the bounding box of the glyph at the caret offset is available.
    // that will do just fine:

    if let Some(atspi_text_iface) = ev_source.get_text_iface() {
        match atspi_text_iface.get_character_extents(ev_detail1, CoordType::Screen) {
            Err(e) => {
                println!("No rect on caret offset, {:?}", &e);
                return;
            }
            Ok(glyph_extents) => {
                cev_cts.set_caret_coords_now(Some(Point(
                    glyph_extents.get_x(),
                    glyph_extents.get_y(),
                )));
            }
        }
    }

    match ev_source.get_id() {
        Ok(atspi_id) => {
            if let Some(id) = cev_cts.get_accessible_id() {
                if id != atspi_id {
                    // found id but of different application
                    // this should not be
                    warn!("different application steals focus");
                    warn!(
                        "focus is claimed by {:?}",
                        ev_source.get_name().unwrap().as_str()
                    );
                }
            } else if cev_cts.get_accessible_id().is_none() {
                cev_cts.set_accessible_id(Some(atspi_id));
            }
        }
        Err(e) => {
            eprintln!("Caret move but no accessbie id, {:?}", e);
            return;
        }
    };
    let atspi_id = cev_cts.get_accessible_id().unwrap();
    // During acquisition of caret events,
    // the origin of the events needs to be the same

    cev_cts.set_pointer_coords_now(obtain_pointer_coords_now(
        cev_conn.clone(),
        screen_num.to_owned(),
    ));

    if cev_cts.move_flag() && cev_cts.get_accessible_id().unwrap() != atspi_id {
        cev_cts.set_glyph_coords_begin(cev_cts.get_caret_coords_now());
        cev_cts.pointer_caret_offset();
        cev_cts.set_move_flag(false);
        cev_cts.set_accessible_id(Some(atspi_id));
        return;
    }

    cev_cts.set_accessible_id(Some(atspi_id));

    let pulse = |_dur| {
        // caret coords now is set
        // pointer coords now is set
        // accessible_id is set

        if !cev_cts.move_flag() {
            cev_cts.set_move_flag(true);

            // Glyph coords information might have been found in a focus event
            // prior to this event
            // if so, we set focus_found_glyph as the begin

            if let Some(focus_glyph) = cev_cts.focus_found_glyph() {
                cev_cts.set_glyph_coords_begin(Some(focus_glyph));
                cev_cts.set_focus_found_glyph(None);
            } else {
                cev_cts.set_glyph_coords_begin(cev_cts.get_caret_coords_now());
            }
            cev_cts.pointer_caret_offset();
        }
    };

    let each_glyph = || {
        if !cev_cts.move_flag() {
            if cev_cts.get_glyph_coords_begin().is_none() {
                cev_cts.set_glyph_coords_begin(cev_cts.get_caret_coords_now());
            }

            // set variable 'pointer_caret_offset' in global state
            // Difference between pointer and glyph at beginposition
            // We use this difference to keep the relative distance
            // between caret and pointer the same.
            // Caveat: no longer works when the mouse is moved
            cev_cts.pointer_caret_offset();
            cev_cts.set_move_flag(true);

            let x =
                cev_cts.get_caret_coords_now().unwrap().0 + cev_cts.get_pointer_caret_offset().0;
            let y =
                cev_cts.get_caret_coords_now().unwrap().1 + cev_cts.get_pointer_caret_offset().1;

            let screen = screen_num.to_owned();
            warp_abs(x, y, cev_conn.clone(), screen);
            cev_cts.set_prev_moved_to(Some(Point(x, y)));
            cev_cts.set_glyph_coords_begin(Some(Point(x, y)));
            return;
        }

        match cev_cts.get_prev_moved_to() {
            Some(expected) if expected != cev_cts.get_pointer_coords_now().unwrap() => {
                // The pointer has been moved by user
                // skip move (postion is up to date)
                // set new pointer and caret begins in state
                // have new pointer_caret_offset calculated
                // wait for new event
                cev_cts.set_glyph_coords_begin(cev_cts.get_caret_coords_now());
                cev_cts.pointer_caret_offset();
                cev_cts.set_move_flag(false);
            }

            Some(expected) if expected == cev_cts.get_pointer_coords_now().unwrap() => {
                // The 'normal'case
                let x = cev_cts.get_caret_coords_now().unwrap().0
                    + cev_cts.get_pointer_caret_offset().0;
                let y = cev_cts.get_caret_coords_now().unwrap().1
                    + cev_cts.get_pointer_caret_offset().1;

                let screen = screen_num.to_owned();

                warp_abs(x, y, cev_conn.clone(), screen);
                cev_cts.set_prev_moved_to(Some(Point(x, y)));
            }
            _ => {}
        }
    };

    match cev_cts.get_behavior() {
        Behavior::Pulse { dur } => {
            pulse(dur);
        }
        Behavior::Typewriter => {
            each_glyph();
        }
    }
}

pub(crate) fn focus_event_processor(
    event: &Event,
    fev_cts: Arc<CaretTowState>,
    fev_conn: Arc<Connection>,
    sn: i32,
) {
    let ev_source: Accessible = event
        .get_source()
        .expect("No Accessible source member on Event type");
    let ev_detail1 = event.get_detail1();

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
    let current_pointer = obtain_pointer_coords_now(fev_conn.clone(), sn).unwrap();

    let update_glyph_and_caret = || {
        // On focus change we want to set the caret coordinates in the global state
        // because this is likely our first opportunity
        &fev_conn.flush();
        if let Some(atspi_text_iface) = ev_source.get_text_iface() {
            match atspi_text_iface.get_character_extents(ev_detail1, CoordType::Screen) {
                Err(e) => {
                    eprintln!("No rect on caret offset, {:?}", &e);
                }
                Ok(glyph_extents) => {
                    fev_cts.set_caret_coords_now(Some(Point(
                        glyph_extents.get_x(),
                        glyph_extents.get_y(),
                    )));
                    fev_cts.set_glyph_coords_begin(Some(Point(
                        glyph_extents.get_x(),
                        glyph_extents.get_y(),
                    )));
                }
            }
        }
    };

    let ev_id: Option<i32> = None;
    if fev_cts.get_accessible_id().is_none() {
        match ev_source.get_id() {
            Ok(ev_id) => {
                fev_cts.set_accessible_id(Some(ev_id));
                update_glyph_and_caret();

                fev_cts.pointer_caret_offset();
                return;
            }
            Err(e) => {
                eprintln!("Error geting accessibles id: {:?}", e);
            }
        }
    }

    if let Some(prev) = fev_cts.get_prev_moved_to() {
        if current_pointer != prev {
            // user caused focus change by moving (mouse) pointer
            // to other editable text of interest
            update_glyph_and_caret();
            fev_cts.set_pointer_coords_now(Some(current_pointer));
            fev_cts.pointer_caret_offset();
            fev_cts.set_accessible_id(ev_id);
        } else {
            // Some other event caused focus change
            // Popup with editable text, we want to go there
            // We may want to go there with sane defaults and set  move
            fev_cts.set_accessible_id(ev_id);
            fev_cts.set_move_flag(false); // induce move
            return;
        }
    }
}
