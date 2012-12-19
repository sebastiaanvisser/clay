{-# LANGUAGE
    OverloadedStrings
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  #-}
module Clay.Background
(
-- * Generic background property.

  Background (background)

-- * The background-color.

, backgroundColor

-- * The background-position.

, BackgroundPosition
, placed
, positioned
, backgroundPosition
, backgroundPositions

-- * The background-size.

, BackgroundSize
, contain, cover
, sized
, backgroundSize
, backgroundSizes

-- * The background-repeat.

, BackgroundRepeat
, repeat, space, round, noRepeat
, xyRepeat
, repeatX, repeatY
, backgroundRepeat
, backgroundRepeats

-- * The background-origin.

, BackgroundOrigin
, origin
, backgroundOrigin
, backgroundOrigins

-- * The background-clip.

, BackgroundClip
, boxClip
, backgroundClip
, backgroundClips

-- * The background-attachment.

, BackgroundAttachment
, attachFixed, attachScroll
, backgroundAttachment
, backgroundAttachments

-- * The background-image.

, BackgroundImage
, url
, linearGradient
, hGradient, vGradient
, backgroundImage
, backgroundImages

-- * Specifying sides.

, Side
, sideTop
, sideLeft
, sideRight
, sideBottom
, sideCenter
, sideMiddle

-- * Specifying direction.

, Direction
, from
, degrees
)
where

import Data.Text (Text, pack)
import Data.Monoid
import Prelude hiding (Left, Right, repeat, round)

import Clay.Box
import Clay.Color
import Clay.Common
import Clay.Property
import Clay.Stylesheet
import Clay.Size

-- | We implement the generic background property as a type class that accepts
-- multiple values types. This allows us to combine different background
-- aspects into a shorthand syntax.

class Val a => Background a where
  background :: a -> Css
  background = key "background"

instance Background a => Background [a]
instance (Background a, Background b) => Background (a, b)

instance Background Color
instance Background BackgroundPosition
instance Background BackgroundSize
instance Background BackgroundRepeat
instance Background BackgroundOrigin
instance Background BackgroundClip
instance Background BackgroundAttachment
instance Background BackgroundImage

-------------------------------------------------------------------------------

backgroundColor :: Color -> Css
backgroundColor = key "background-color"

-------------------------------------------------------------------------------

newtype BackgroundPosition = BackgroundPosition Value
  deriving (Val, Other, Inherit)

placed :: Side -> Side -> BackgroundPosition
placed a b = BackgroundPosition (value (a, b))

positioned :: Size a -> Size a -> BackgroundPosition
positioned a b = BackgroundPosition (value (a, b))

backgroundPosition :: BackgroundPosition -> Css
backgroundPosition = key "background-position"

backgroundPositions :: [BackgroundPosition] -> Css
backgroundPositions = key "background-position"

-------------------------------------------------------------------------------

newtype BackgroundSize = BackgroundSize Value
  deriving (Val, Other, Inherit)

instance Auto BackgroundSize where auto = sized auto auto

contain, cover :: BackgroundSize

contain = BackgroundSize "contain"
cover   = BackgroundSize "cover"

sized :: Size a -> Size a -> BackgroundSize
sized a b = BackgroundSize (value (a, b))

backgroundSize :: BackgroundSize -> Css
backgroundSize = key "background-size"

backgroundSizes :: [BackgroundSize] -> Css
backgroundSizes = key "background-size"

-------------------------------------------------------------------------------

newtype BackgroundRepeat = BackgroundRepeat Value
  deriving (Val, Other, Inherit, None)

repeat, space, round, noRepeat :: BackgroundRepeat

repeat   = BackgroundRepeat "repeat"
space    = BackgroundRepeat "space"
round    = BackgroundRepeat "round"
noRepeat = BackgroundRepeat "no-repeat"

xyRepeat :: BackgroundRepeat -> BackgroundRepeat -> BackgroundRepeat
xyRepeat a b = BackgroundRepeat (value (a, b))

repeatX, repeatY :: BackgroundRepeat

repeatX = xyRepeat repeat noRepeat
repeatY = xyRepeat noRepeat repeat

backgroundRepeat :: BackgroundRepeat -> Css
backgroundRepeat = key "background-repeat"

backgroundRepeats :: [BackgroundRepeat] -> Css
backgroundRepeats = key "background-repeat"

-------------------------------------------------------------------------------

newtype BackgroundImage = BackgroundImage Value
  deriving (Val, Other, Inherit, None)

url :: Text -> BackgroundImage
url u = BackgroundImage (value ("url(\"" <> u <> "\")"))

linearGradient :: Direction -> [(Color, Size Rel)] -> BackgroundImage
linearGradient d xs = BackgroundImage $ Value $
  let Value v = "linear-gradient(" <> value d <> "," <> value (map (\(a, b) -> value (value a, value b)) xs) <> ")"
  in browsers <> v

hGradient, vGradient :: Color -> Color -> BackgroundImage

hGradient f t = linearGradient (from sideLeft) [(f, 0), (t, 100)]
vGradient f t = linearGradient (from sideTop ) [(f, 0), (t, 100)]

backgroundImage :: BackgroundImage -> Css
backgroundImage = key "background-image"

backgroundImages :: [BackgroundImage] -> Css
backgroundImages = key "background-image"

-------------------------------------------------------------------------------

newtype BackgroundOrigin = BackgroundOrigin Value
  deriving (Val, Other, Inherit)

origin :: BoxType -> BackgroundOrigin
origin b = BackgroundOrigin (value b)

backgroundOrigin :: BackgroundOrigin -> Css
backgroundOrigin = key "background-origin"

backgroundOrigins :: [BackgroundOrigin] -> Css
backgroundOrigins = key "background-origin"

-------------------------------------------------------------------------------

newtype BackgroundClip = BackgroundClip Value
  deriving (Val, Other, Inherit)

boxClip :: BoxType -> BackgroundClip
boxClip b = BackgroundClip (value b)

backgroundClip :: BackgroundClip -> Css
backgroundClip = key "background-clip"

backgroundClips :: [BackgroundClip] -> Css
backgroundClips = key "background-clip"

-------------------------------------------------------------------------------

newtype BackgroundAttachment = BackgroundAttachment Value
  deriving (Other, Val, Inherit)

attachFixed, attachScroll :: BackgroundAttachment
attachFixed  = BackgroundAttachment "fixed"
attachScroll = BackgroundAttachment "scroll"

backgroundAttachment :: BackgroundAttachment -> Css
backgroundAttachment = key "background-attachment"

backgroundAttachments :: [BackgroundAttachment] -> Css
backgroundAttachments = key "background-attachment"

-------------------------------------------------------------------------------

newtype Side = Side Value
  deriving (Val, Other, Inherit)

-- | We have to prefix these values to avoid conflict with existing property
-- names.

sideTop, sideLeft, sideRight, sideBottom, sideCenter, sideMiddle :: Side

sideTop    = Side "top"
sideLeft   = Side "left"
sideRight  = Side "right"
sideBottom = Side "bottom"
sideCenter = Side "center"
sideMiddle = Side "middle"

-------------------------------------------------------------------------------

newtype Direction = Direction Value
  deriving (Val, Other)

from :: Side -> Direction
from a = Direction (value a)

degrees :: Double -> Direction
degrees a = Direction (value (pack (show a) <> "deg"))

