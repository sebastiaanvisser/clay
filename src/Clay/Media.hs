{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Clay.Media
(

-- * Media types.

  all, aural, braille, handheld, print, projection
, screen, tty, tv, embossed

-- * Geometrical features.

, width, minWidth, maxWidth, height, minHeight, maxHeight, deviceWidth
, minDeviceWidth, maxDeviceWidth, deviceHeight, minDeviceHeight
, maxDeviceHeight

-- * Aspect ration features.

, aspectRatio, minAspectRatio, maxAspectRatio, deviceAspectRatio
, minDeviceAspectRatio, maxDeviceAspectRatio

-- * Color related features.

, color, monochrome, scan, grid
, minColor, maxColor, colorIndex, minColorIndex, maxColorIndex, minMonochrome
, maxMonochrome

-- * Resolution related features.

, resolution, minResolution, maxResolution

-- * Resolution value type.

, Resolution
, dpi
, dppx
)

where

import Data.Text (Text, pack)
import Data.Monoid

import Clay.Common
import Clay.Size
import Clay.Property
import Clay.Stylesheet

import Prelude hiding (all, print)

-------------------------------------------------------------------------------

all, aural, braille, handheld, print, projection
  , screen, tty, tv, embossed :: MediaType

all        = MediaType "all"
aural      = MediaType "aural"
braille    = MediaType "braille"
handheld   = MediaType "handheld"
print      = MediaType "print"
projection = MediaType "projection"
screen     = MediaType "screen"
tty        = MediaType "tty"
tv         = MediaType "tv"
embossed   = MediaType "embossed"

-------------------------------------------------------------------------------

with :: Val a => Text -> a -> Feature
with f v = Feature f (Just (value v))

without :: Text -> Feature
without f = Feature f Nothing

width, minWidth, maxWidth, height, minHeight, maxHeight, deviceWidth
  , minDeviceWidth, maxDeviceWidth, deviceHeight, minDeviceHeight
  , maxDeviceHeight :: Size Abs -> Feature

width           = with "width"
minWidth        = with "min-width"
maxWidth        = with "max-width"
height          = with "height"
minHeight       = with "min-height"
maxHeight       = with "max-height"
deviceWidth     = with "device-width"
minDeviceWidth  = with "min-device-width"
maxDeviceWidth  = with "max-device-width"
deviceHeight    = with "device-height"
minDeviceHeight = with "min-device-height"
maxDeviceHeight = with "max-device-height"

aspectRatio, minAspectRatio, maxAspectRatio, deviceAspectRatio
  , minDeviceAspectRatio, maxDeviceAspectRatio :: (Integer, Integer) -> Feature

aspectRatio          (x, y) = with "aspect-ratio"            (value x <> "/" <> value y)
minAspectRatio       (x, y) = with "min-aspect-ratio"        (value x <> "/" <> value y)
maxAspectRatio       (x, y) = with "max-aspect-ratio"        (value x <> "/" <> value y)
deviceAspectRatio    (x, y) = with "device-aspect-ratio"     (value x <> "/" <> value y)
minDeviceAspectRatio (x, y) = with "min-device-aspect-ratio" (value x <> "/" <> value y)
maxDeviceAspectRatio (x, y) = with "max-device-aspect-ratio" (value x <> "/" <> value y)

color, monochrome, scan, grid :: Feature

color      = without "color"
monochrome = without "monochrome"
scan       = without "scan"
grid       = without "grid"

minColor, maxColor, colorIndex, minColorIndex, maxColorIndex, minMonochrome
  , maxMonochrome :: Integer -> Feature

minColor      = with "min-color"
maxColor      = with "max-color"
colorIndex    = with "color-index"
minColorIndex = with "min-color-index"
maxColorIndex = with "max-color-index"
minMonochrome = with "min-monochrome"
maxMonochrome = with "max-monochrome"

resolution, minResolution, maxResolution :: Val a => a -> Feature

resolution    = with "resolution"
minResolution = with "min-resolution"
maxResolution = with "max-resolution"

-------------------------------------------------------------------------------

newtype Resolution = Resolution Value
  deriving (Val, Other)

dpi :: Integer -> Resolution
dpi i = Resolution (value (pack (show i) <> "dpi"))

dppx :: Integer -> Resolution
dppx i = Resolution (value (pack (show i) <> "dppx"))

