Codec.Pictures
==============

This library provide saving & loading of different picture formats for
the Haskell language. The aim of the library is to be as lightweight as
possible, you ask it to load an image, and it'l dump you a big MUArray 
or UArray full of juicy pixels. Or squared pixels, or whatever, as long
as they're unboxed.

Status
------

 - PNG    (.png) loading, fully compliant with the standard, tested against png suite.
 - Bitmap (.bmp) Writing of 32bits per pixel images, mainly used as a debug output format.
 - Jpeg   (.jpg, .jpeg) work in progress.

_I love juicy pixels_

