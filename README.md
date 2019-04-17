# tow

    an ergonomy helper for desktop zoom users.
    Adding a little extra convenience to zoom.

Tow has the zoom area be 'towed by the keyboard caret'.

(The caret is possibly better known as 'text-cursor')

Tow is written to work with xfwm4 of the Xfce4 desktop,
however it might work with other desktop environments as well.
Your mileage may vary.

Tow seeks to scratch an itch some users who depend on the zoom
feature may have while working with zoom.

## Problem

 The user directs the zoomed view port position with the pointer device.
 (eg. the mouse, or any other pointer device.)

 Typically both hands are in use when typing and most users
 have no more than two hands.
 Whilst entering text, the caret moves, yet the zoomed view port stays put.
 Quickly the caret will be out of sight and the user now has to
 interrupt work to readjust the pointer position to once again have
 the zoomed view port match the caret's current position.

 This pattern will repeat and becomes a nuisance to some.

 for those people I wrote tow.

## The solution

 Tow aims to automate the readjusting by having the zoom view port
 be 'towed' by the caret.

## Usage

    tow [FLAGS] [OPTIONS]

FLAGS:
    -D, --daemon     Have tow be 'daemonized' / run in the background.
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -b, --behavior [ mode ] [ val ]    Mode: 'charcnt' [N:2-100] (# chars), 'interval' [N: 100-10000] (ms) or 'typewriter' [ - ] (default)

    -s, --slide_duration [dur. in ms]  Duration of view port slide in ms [100-10000] only applies to charcnt and
                                       interval modes, otherwise ignored [default: 500]

## TO DO

- Tow 'works' but it has (many) flaws.

    Tow is not stable, let alone ready for a release.

    The main problems as of [2019-04-08]:

    Tow needs a safe wrapper for its ffi use.
    I am pretty sure that in its current state tow is leaking memory.
    Memory management is currently poorly understood by its author.

    Tow also needs a runtime on/off hotkey.
Some programs do not implement accessibility as intended, this may lead to unwanted behavior.
Therefore we need and option to opt-out during runtime.

    We probably want to save settings.

    Even though we have greatly improved event handling speed, events can still be missed (at-spi2 may decide to drop events) and we may have 'false positive' caret-moved events - these may cause unwanted behavior.

    Tow may get its own worker thread to do the moving while the main thread does the acquisition and accounting.
    Currently [2019-04-08] we move at the first event that comes after the the set interval duration.
    The current implementation makes the interval duration less meaningful.

    Detect and 'play nice' with policy kit.
    Which may be as simple as prefixing with pk-exec?

### Caveats / bugs

    [2019-04-16] Until we have a worker thread, slide times must be smaller than move (acquisition) times.

    [2019-04-16] Until tow can discern synthetic from device (mouse / pad / other) moves, inbetween moves lead to unwanted moves.

## Installation

prerequisites include
. Rust development toolchain.
. at-spi2-core

 $ git clone <https://github.com/luukvanderduim/tow.git>

 $ cd tow

 $ cargo build --release

 $ cargo run --release
 or

 $ cargo install --path .  (add --force on subsequent invocation)

 $ tow

## Contributions

 Yes, please!

 If you can fix problems with tow - or - take accerciser to check your (app of interest)
 for Atk implementation shortcomings. Lets improve our ecosystem!

 I would really love to see VSCode emit caret-moved events.

 I would like to know how to pursuade Thunderbird to emit caret-moved events. Anyone?

 Thank you so much in advance.

## Contact

Don't hesitate to drop a line at:
Especially if you use tow and like to share your experiences.

luukvanderduim (-AT-) gmail (-dot-) com

## Licence

This software is licenced MPL 2.0
Please see the LICENCE file.
