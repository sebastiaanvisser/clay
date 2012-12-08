{-# LANGUAGE OverloadedStrings, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Clay.Background where

import Data.Text (Text, pack)
import Data.Monoid
import Prelude hiding (Left, Right, repeat)

import Clay.Property
import Clay.Rule
import Clay.Color
import Clay.Common
import Clay.Size

-- Background property as a type class.

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

newtype Side = Side Value
  deriving Val

instance Inherit Side where inherit = Side "inherit"
instance Other   Side where other   = Side

pTop, pLeft, pRight, pBottom, pCenter, pMiddle :: Side

pTop    = Side "top"
pLeft   = Side "left"
pRight  = Side "right"
pBottom = Side "bottom"
pCenter = Side "center"
pMiddle = Side "middle"

newtype BackgroundPosition = BackgroundPosition Value
  deriving Val

placed :: Side -> Side -> BackgroundPosition
placed a b = BackgroundPosition (value (a, b))

positioned :: Size a -> Size a -> BackgroundPosition
positioned a b = BackgroundPosition (value (a, b))

instance Inherit BackgroundPosition where inherit = BackgroundPosition "inherit"
instance Other   BackgroundPosition where other   = BackgroundPosition

backgroundPosition :: BackgroundPosition -> Css
backgroundPosition = key "background-position"

backgroundPositions :: [BackgroundPosition] -> Css
backgroundPositions = key "background-position"

-------------------------------------------------------------------------------

newtype BackgroundSize = BackgroundSize Value
  deriving Val

contain, cover :: BackgroundSize

contain = BackgroundSize "contain"
cover   = BackgroundSize "cover"

sized :: Size a -> Size a -> BackgroundSize
sized a b = BackgroundSize (value (a, b))

instance Inherit BackgroundSize where inherit = BackgroundSize "inherit"
instance Auto    BackgroundSize where auto    = sized auto auto
instance Other   BackgroundSize where other   = BackgroundSize

backgroundSize :: BackgroundSize -> Css
backgroundSize = key "background-size"

backgroundSizes :: [BackgroundSize] -> Css
backgroundSizes = key "background-size"

-------------------------------------------------------------------------------

newtype BackgroundRepeat = BackgroundRepeat Value
  deriving Val

instance Other BackgroundRepeat where other = BackgroundRepeat

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
  deriving Val

instance Other BackgroundImage where other = BackgroundImage
instance None  BackgroundImage where none  = BackgroundImage "none"

url :: Text -> BackgroundImage
url u = BackgroundImage (value ("url(\"" <> u <> "\")"))

newtype Direction = Direction Value
  deriving Val

from :: Side -> Direction
from a = Direction (value a)

degrees :: Double -> Direction
degrees a = Direction (value (pack (show a) <> "deg"))

linearGradient :: Direction -> [(Color, Size Rel)] -> BackgroundImage
linearGradient d xs = BackgroundImage $ Value $
  let Value v = "linear-gradient(" <> value d <> "," <> value (map (\(a, b) -> value (value a, value b)) xs) <> ")"
  in Prefixed [ ("-webkit-", plain v)
              , ("-moz-",    plain v)
              ]

hGradient, vGradient :: Color -> Color -> BackgroundImage

hGradient f t = linearGradient (from pLeft) [(f, 0), (t, 100)]
vGradient f t = linearGradient (from pTop) [(f, 0), (t, 100)]

backgroundImage :: BackgroundImage -> Css
backgroundImage = key "background-image"

backgroundImages :: [BackgroundImage] -> Css
backgroundImages = key "background-image"

-------------------------------------------------------------------------------

newtype BoxType = BoxType Value
  deriving Val

paddingBox, borderBox, contentBox :: BoxType

paddingBox = BoxType "padding-box"
borderBox  = BoxType "border-box"
contentBox = BoxType "content-box"

newtype BackgroundOrigin = BackgroundOrigin Value
  deriving Val

origin :: BoxType -> BackgroundOrigin
origin b = BackgroundOrigin (value b)

backgroundOrigin :: BackgroundOrigin -> Css
backgroundOrigin = key "background-origin"

backgroundOrigins :: [BackgroundOrigin] -> Css
backgroundOrigins = key "background-origin"

-------------------------------------------------------------------------------

newtype BackgroundClip = BackgroundClip Value
  deriving Val

clip :: BoxType -> BackgroundClip
clip b = BackgroundClip (value b)

backgroundClip :: BoxType -> Css
backgroundClip = key "background-clip"

backgroundClips :: [BoxType] -> Css
backgroundClips = key "background-clip"

-------------------------------------------------------------------------------

newtype BackgroundAttachment = BackgroundAttachment Value
  deriving Val

instance Inherit BackgroundAttachment where inherit = BackgroundAttachment "inherit"

attachFixed, attachScroll :: BackgroundAttachment
attachFixed  = BackgroundAttachment "fixed"
attachScroll = BackgroundAttachment "scroll"

backgroundAttachment :: BackgroundAttachment -> Css
backgroundAttachment = key "background-attachment"

backgroundAttachments :: [BackgroundAttachment] -> Css
backgroundAttachments = key "background-attachment"

