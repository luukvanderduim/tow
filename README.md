[![Build Status](https://travis-ci.com/luukvanderduim/tow.svg?branch=master)](https://travis-ci.com/luukvanderduim/tow)

![Tow Logo](https://github.com/luukvanderduim/tow/blob/master/img/tugboat.png "Tow logo by Coenraad E. Meijer")

Tow and its accompanying tug boat image are very much works in progress.

# tow

    an ergonomy helper for desktop zoom users.
    Adding a little extra convenience to zoom.

Tow has the zoom area be 'towed' by the keyboard caret.

('caret' is possibly better known as 'text-cursor')

Tow is written to work with xfwm4 of the Xfce4 desktop,
however it might work with other desktop environments as well.
Your mileage may vary.

Users who depend on desktop zoom may appreciate tow.
We want zoom but don't want it to be in our way.

If you think tow is cool convenience and want to help out, please consider buying me a coffee! ;)

[![ko-fi](https://www.ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/X8X116XGP)

## The problem tow solves
 Zoom users zoom-in a portion of their screen to magnify a region of interest.
 Whilst typing text, the text cursor (caret) moves, yet the zoomed view area stays put. 
 Consequently the text cursor will be out of sight before soon, forcing the user to stop working to readjust the view area's position using the mouse or some other pointer device.
 
 This pattern will repeat and becomes a nuisance to some.


## The solution

 Tow aims to automate the readjusting by having the zoom view port (the magnified area)
 be 'towed' by the text cursor (caret).

 Applications that use a widget toolkit that supports AT-SPI2 (such as GTK or Qt),
 can share information regarding the carets position.

## Usage

    tow [FLAGS] [OPTIONS]

FLAGS:
    -D, --daemon     Have tow be 'daemonized' / run in the background.
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -b, --behavior [ mode ] [ val ]    Mode: 'pulse' [N: 100-10000] (ms) or 'typewriter' (default)

    -s, --slide_duration [dur. in ms]  Duration of view port slide in ms [100-10000] only applies to pulse mode, otherwise ignored [default: 500]

## TO DO

- Tow 'works' but it has (many) flaws.

    Tow is unstable.
        We might want a cleaner state machine
        Your author would like all of the generate atspi ffi interface to be safe
    
    Your author would like CI on this repository.

    The main problems:

    Tow needs a runtime on/off hotkey.

Some programs do not implement accessibility as intended, this may lead to unwanted behavior.
Therefore we need and option to opt-out during runtime.

    We probably want to save settings at some point.

    Detect and 'play nice' with policy kit.
    Which may be as simple as prefixing with pk-exec?

### Caveats / bugs

    [2019-04-16] Until tow can discern synthetic from device (mouse / pad / other) moves, inbetween moves lead to unwanted moves.

    [2019-10-30] Sometimes tow makes an unexpected move. Need to find the origin of this.

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

luukvanderduim (-AT-) gmail (-dot-) com

## Licence

This software is licenced MPL 2.0
Please see the LICENCE file.
