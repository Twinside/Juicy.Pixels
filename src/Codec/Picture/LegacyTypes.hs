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
module Codec.Picture.LegacyTypes
                          ( -- * Types
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
                          , PixelYA8( .. )
                          , PixelYA16( .. )
                          , PixelRGB8( .. )
                          , PixelRGB16( .. )
                          , PixelRGBF( .. )
                          , PixelRGBA8( .. )
                          , PixelRGBA16( .. )
                          , PixelCMYK8( .. )
                          , PixelCMYK16( .. )
                          , PixelYCbCr8( .. )
                          , PixelYCbCrK8( .. )

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
import Codec.Picture.OldPixels
{-import Codec.Picture.Lenses-}

