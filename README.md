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

## Problem description
 The user directs the zoom view port to some (text input)
 area by means of eg. the mouse. ( or any other pointer device ).

 Typically both hands are in use when typing and most users
 have no more than two hands.
 While entering text, the caret moves, yet the zoom view stays put.
 Quickly the caret will be out of sight and the user now has to
 interrupt work to readjust the pointer position to once again have
 the zoomed view port match the caret's current position.
 But not for long.

## The solution
 Tow aims to automate the readjusting by having the zoom view port
 be 'towed' by the caret.

TO DO:
- It works somewhat, but it has (many) flaws.
  It is by no means 'stable' let alone 'done', far from a release.

Events may be missed, we may have 'false positive' caret move events
There is unwanted behavior

 - non blocking mover.
 - detect and 'play nice' with policy kit.
 - ..

## How to use

prerequisites include
. Rust development toolchain.
. libatspi

 $ git clone https://github.com/luukvanderduim/tow.git
 $ cd tow
 $ cargo build --release
 $ cargo run --release

 tow is a daemon and currently has no way to gracefully exit.
 So you may want to kill it when done experimenting with it.

## Contributions

 Yes, please!

 (Keep in mind all code contributed will be licenced like tow is.)