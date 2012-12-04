{-# LANGUAGE OverloadedStrings, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Clay.Style.Background where

import Data.Text (Text, pack)
import Data.Monoid
import Prelude hiding (Left, Right, repeat)

import Clay.Core.Property
import Clay.Core.Rule
import Clay.Style.Color
import Clay.Style.Common
import Clay.Style.Size

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

repeatX :: BackgroundRepeat
repeatX = xyRepeat repeat noRepeat

repeatY :: BackgroundRepeat
repeatY = xyRepeat noRepeat repeat

backgroundRepeat :: BackgroundRepeat -> Css
backgroundRepeat = key "background-repeat"

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
linearGradient d xs = BackgroundImage $
  "-webkit-linear-gradient(" <> value d <> "," <> value (map (\(a, b) -> value (value a, value b)) xs) <> ")"

hGradient :: Color -> Color -> BackgroundImage
hGradient f t = linearGradient (from pLeft) [(f, 0), (t, 100)]

vGradient :: Color -> Color -> BackgroundImage
vGradient f t = linearGradient (from pTop) [(f, 0), (t, 100)]

backgroundImage :: BackgroundImage -> Css
backgroundImage = key "background-image"

backgroundImages :: [BackgroundImage] -> Css
backgroundImages = key "background-image"

-------------------------------------------------------------------------------

-- backgroundOrigin     = key "background-origin"
-- backgroundClip       = key "background-clip"
-- backgroundAttachment = key "background-attachment"

-------------------------------------------------------------------------------
-- Background property as a type class.

class Val a => Background a where
  background :: a -> Css
  background = key "background"

instance Background Color
instance Background BackgroundPosition
instance Background BackgroundImage
instance Background BackgroundSize
instance Background BackgroundRepeat
instance Background [BackgroundPosition]
instance Background [BackgroundImage]
instance Background [BackgroundSize]
instance Background [BackgroundRepeat]

