{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Clay.Filter
(
-- * Filter property.

  Filter
, filter
, filters

-- * Specific filter functions.

, url
, blur
, brightness
, contrast
, dropShadow
, grayscale
, hueRotate
, invert
, opacity
, saturate
, sepia
)
where

import Data.Monoid
import Data.Text hiding (filter)
import Prelude hiding (filter)

import Clay.Color
import Clay.Common
import Clay.Property
import Clay.Stylesheet
import Clay.Size

-------------------------------------------------------------------------------

newtype Filter = Filter Value
  deriving (Val, None, Inherit)

filter :: Filter -> Css
filter = prefixed (browsers <> "filter")

filters :: [Filter] -> Css
filters x = prefixed (browsers <> "filter") (noCommas x)

-------------------------------------------------------------------------------

url :: Text -> Filter
url u = Filter ("url(" <> value u <> ")")

blur :: Size Abs -> Filter
blur i = Filter ("blur(" <> value i <> ")")

brightness :: Double -> Filter
brightness i = Filter ("brightness(" <> value i <> ")")

contrast :: Size Rel -> Filter
contrast i = Filter ("contrast(" <> value i <> ")")

dropShadow :: Size Abs -> Size Abs -> Size Abs -> Color -> Filter
dropShadow x y s c = Filter ("drop-shadow(" <> value (x ! y ! s ! c) <> ")")

grayscale :: Size Rel -> Filter
grayscale g = Filter ("grayscale(" <> value g <> ")")

hueRotate :: Angle a -> Filter
hueRotate h = Filter ("hue-rotate(" <> value h <> ")")

invert :: Size Rel -> Filter
invert i = Filter ("invert(" <> value i <> ")")

opacity :: Size Rel -> Filter
opacity i = Filter ("opacity(" <> value i <> ")")

saturate :: Size Rel -> Filter
saturate i = Filter ("saturate(" <> value i <> ")")

sepia :: Size Rel -> Filter
sepia i = Filter ("sepia(" <> value i <> ")")

