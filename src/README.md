# tow

Seeks to scratch an itch users who depend on the zoom feature
may have with the zoom feature of xfwm4.

Currently tow does nearly nothing, it moves the mouse pointer.

# The problem:
 A user directs the Xfce zoom view port to some (text input)
 area by means of eg. a mouse device. ( or any other pointer device of choice ).
 Typically both hands are in use when typing. The caret (keyboard cursor)
 is moving while the view port stays put until the user decides to move the pointer
 to match  the caret's position.

 Tow aims to reduce the amount of manual zoom area repositioning by
 having it be 'towed' by the caret.

TO DO:
 - everything:
 - add AT-SPI2 based caret position tracking and accounting.
 - detect and 'play nice' with policy kit.
 - daemonize
 - lots more


