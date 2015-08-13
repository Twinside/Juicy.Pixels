{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
-- | Module providing the basic types for image manipulation in the library.
-- Defining the types used to store all those _Juicy Pixels_
module Codec.Picture.Types( -- * Types
                            -- ** Image types
                            Image( .. )
                          , MutableImage( .. )
                          , DynamicImage( .. )
                          , Palette

                            -- ** Image functions
                          , createMutableImage
                          , newMutableImage
                          , freezeImage
                          , unsafeFreezeImage
                          , thawImage
                          , unsafeThawImage

                            -- ** Image Lenses
                          , Traversal
                          , imagePixels
                          , imageIPixels

                            -- ** Pixel types
                          , Pixel8
                          , Pixel16
                          , Pixel32
                          , PixelF
                          , YA( .. )
                          , RGB( .. )
                          , RGBA( .. )
                          , CMYK( .. )
                          , YCbCr( .. )
                          , YCbCrK( .. )

                          -- * Type classes
                          , ColorConvertible( .. )
                          , Pixel(..)
                          -- $graph
                          , ColorSpaceConvertible( .. )
                          , LumaPlaneExtractable( .. )
                          , TransparentPixel( .. )

                            -- * Helper functions
                          , pixelMap
                          , pixelMapXY
                          , pixelFold
                          , pixelFoldM
                          , pixelFoldMap

                          , dynamicMap
                          , dynamicPixelMap
                          , dropAlphaLayer
                          , withImage
                          , zipPixelComponent3
                          , generateImage
                          , generateFoldImage
                          , gammaCorrection
                          , toneMapping

                            -- * Color plane extraction
                          , ColorPlane ( )

                          , PlaneRed( .. )
                          , PlaneGreen( .. )
                          , PlaneBlue( .. )
                          , PlaneAlpha( .. )
                          , PlaneLuma( .. )
                          , PlaneCr( .. )
                          , PlaneCb( .. )
                          , PlaneCyan( .. )
                          , PlaneMagenta( .. )
                          , PlaneYellow( .. )
                          , PlaneBlack( .. )

                          , extractComponent
                          , unsafeExtractComponent

                            -- * Packeable writing (unsafe but faster)
                          , PackeablePixel( .. )
                          , fillImageWith
                          , readPackedPixelAt
                          , writePackedPixelAt
                          , unsafeWritePixelBetweenAt
                          ) where

import Codec.Picture.BaseTypes
import Codec.Picture.NewPixels
{-import Codec.Picture.Lenses-}

