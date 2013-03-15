![Juicy logo](http://twinside.github.com/Juicy.Pixels/juicy.png)

[![Build Status](https://travis-ci.org/Twinside/Juicy.Pixels.png?branch=master)](https://travis-ci.org/Twinside/Juicy.Pixels)

Juicy.Pixels
============

This library provide saving & loading of different picture formats for the
Haskell language. The aim of the library is to be as lightweight as possible,
you ask it to load an image, and it'l dump you a big Vector full of juicy
pixels. Or squared pixels, or whatever, as long as they're unboxed.

Documentation
-------------
The library documentation can be accessed on [Hackage](http://hackage.haskell.org/package/JuicyPixels)

REPA
----
For the user of
[REPA](http://www.haskell.org/haskellwiki/Numeric_Haskell:_A_Repa_Tutorial),
check-out JuicyPixels-repa on
[GitHub](https://github.com/TomMD/JuicyPixels-repa) or
[Hackage](http://hackage.haskell.org/package/JuicyPixels-repa)

Status
------

 - PNG    (.png) 
    * Reading 
        - 1,2,4,8 bits loading, Grayscale, 24bits, 24 bits with alpha,
          interleaved & filtered (fully compliant with the standard,
          tested against png suite).

    * Writing
        - 8bits RGB (non interleaved)
        - 8bits RGBA (non interleaved)
        - 8bits greyscale (non interleaved)

 - Bitmap (.bmp) (mainly used as a debug output format)
    * Reading
        - 24bits (RGB) images

    * Writing
        - 32bits (RGBA) per pixel images
        - 24bits (RGB) per pixel images
        - 8 bits greyscale (with palette)

 - Jpeg   (.jpg, .jpeg) 
    * Reading non-interlaced baseline DCT image, seems to be OK
    * Writing

 - Gif (.gif)
    * Reading single image & animated Gif image, handles interlaced images.

 - Radiance (.pic, .hdr)
    * Reading
    * Writing

_I love juicy pixels_

You can make [donations on this page](http://twinside.github.com/Juicy.Pixels/).

