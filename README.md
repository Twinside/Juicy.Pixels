![Juicy logo](https://raw.github.com/Twinside/Juicy.Pixels/master/docimages/juicy.png)


[![Build Status](https://travis-ci.org/Twinside/Juicy.Pixels.png?branch=master)](https://travis-ci.org/Twinside/Juicy.Pixels)
[![Hackage](https://img.shields.io/hackage/v/JuicyPixels.svg)](http://hackage.haskell.org/package/JuicyPixels)

Juicy.Pixels
============

This library provides saving & loading of different picture formats for the
Haskell language. The aim of the library is to be as lightweight as possible,
you ask it to load an image, and it'll dump you a big Vector full of juicy
pixels. Or squared pixels, or whatever, as long as they're unboxed.

Documentation
-------------

The library documentation can be accessed on [Hackage](http://hackage.haskell.org/package/JuicyPixels)

Wrappers
--------

For the user of:

 * [REPA](http://www.haskell.org/haskellwiki/Numeric_Haskell:_A_Repa_Tutorial), check-out JuicyPixels-repa on [GitHub](https://github.com/TomMD/JuicyPixels-repa) or [Hackage](http://hackage.haskell.org/package/JuicyPixels-repa)

 * [Gloss](http://hackage.haskell.org/package/gloss), check-out gloss-juicy on [GitHub](https://github.com/alpmestan/gloss-juicy) or [Hackage](http://hackage.haskell.org/package/gloss-juicy)

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
        - 16bits greyscale (non interleaved)
        - 16bits RGB (non interleaved)
        - 16bits RGBA (non interleaved)

    * Metadata (reading/writing)
        * in a tEXT chunk: 'Title', 'Description', 'Author', 'Copyright',
          'Software', 'Comment', 'Disclaimer', 'Source', 'Warning'
        * any other tEXT chunk.
        * in a gAMA field : 'Gamma'
        * DPI information in a pHYs chunk.

 - Bitmap (.bmp) (mainly used as a debug output format)
    * Reading
        - 32bits (RGBA) images
        - 24bits (RGB) images
        - 8bits (greyscale & paletted) images

    * Writing
        - 32bits (RGBA) per pixel images
        - 24bits (RGB) per pixel images
        - 8 bits greyscale (with palette)

    * Metadata (reading/writing): DPI information

 - Jpeg   (.jpg, .jpeg)
    * Reading normal and interlaced baseline DCT image
        - YCbCr (default) CMYK/YCbCrK/RGB colorspaces

    * Writing non-interlaced JPG
        - YCbCr (favored), Y, RGB & CMYK colorspaces

    * Metadata:
        - Reading and writing DpiX & DpiY from JFIF header.
        - Reading EXIF metadata.

 - Gif (.gif)
    * Reading single image & animated Gif image, handles interlaced images.
    * Writing single & animated Gif images.
    * No metadata.

 - Radiance (.pic, .hdr)
    * Reading
    * Writing
    * No metadata.

 - Tga
    * Reading
        - 8, 16, 24 & 32 bits
        - paletted and unpaletted
        - RLE encoded or uncompressed
    * Writing
        - uncompressed 8bits (Pixel8)
        - uncompressed 24bits (PixelRGB8)
        - uncompressed 32bits (PixelRGBA8)
    * No metadata

 - Tiff
    * Reading
        - 2, 4, 8, 16 bit depth reading (planar and contiguous for each)
        - CMYK, YCbCr, RGB, RGBA, Paletted, Greyscale
        - Uncompressed, PackBits, LZW

    * Writing
        - 8 and 16 bits
        - CMYK, YCbCr, RGB, RGBA, Greyscale
        - Uncompressed
    * Metadata: reading DpiX, DpiY and EXIF informations.

_I love juicy pixels_

You can make [donations on this page](http://twinside.github.com/Juicy.Pixels/).

