{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Style where

import Data.Text (Text)
import Data.Monoid
import Data.Maybe
import Prelude hiding (Left, Right)

import Color
import Size
import Property
import Rule

-------------------------------------------------------------------------------

size, top, left, bottom, right, width, height :: Size -> Css
size   = key "size"
top    = key "top"
left   = key "left"
bottom = key "bottom"
right  = key "right"
width  = key "width"
height = key "height"

-------------------------------------------------------------------------------

padding, margin :: Size -> Size -> Size -> Size -> Css
padding = key4 "padding"
margin  = key4 "margin"

paddingTop, paddingLeft, paddingRight, paddingBottom,
  marginTop, marginLeft, marginRight, marginBottom :: Size -> Css

paddingTop    = key "paddingTop"
paddingLeft   = key "paddingLeft"
paddingRight  = key "paddingRight"
paddingBottom = key "paddingBottom"
marginTop     = key "marginTop"
marginLeft    = key "marginLeft"
marginRight   = key "marginRight"
marginBottom  = key "marginBottom"

sym :: (Size -> Size -> Size -> Size -> Css) -> Size -> Size -> Css
sym k a b = k a b a b

-------------------------------------------------------------------------------

font :: Text -> Size -> Color -> Css
font = key3 "font"

fontFamily :: Text -> Css
fontFamily = key "font-family"

fontSize :: Size -> Css
fontSize = key "font-family"

color :: Color -> Css
color = key "color"

fontColor :: Color -> Css
fontColor = key "color"

-------------------------------------------------------------------------------

data Side = Top | Left | Right | Bottom | Center

instance Val Side where
  value Top    = "top"
  value Left   = "left"
  value Right  = "right"
  value Bottom = "bottom"
  value Center = "center"

data BackgroundPosition
  = Place Side Side
  | Size  Size Size
  | Inherit

instance Val BackgroundPosition where
  value (Place a b) = value (a, b)
  value (Size  a b) = value (a, b)
  value Inherit     = "inherit"

data BackgroundSize
  = Contain
  | Cover
  | Length (Maybe Size) (Maybe Size)

instance Val BackgroundSize where
  value Contain      = "contain"
  value Cover        = "cover"
  value (Length a b) = value (maybe "auto" value a, maybe "auto" value b)

data BackgroundRepeat
  = Repeat
  | Space
  | Round
  | NoRepeat

instance Val BackgroundRepeat where
  value Repeat   = "repeat"
  value Space    = "space"
  value Round    = "round"
  value NoRepeat = "no-repeat"

repeat :: (BackgroundRepeat, BackgroundRepeat)
repeat = (Repeat, Repeat)

repeatX :: (BackgroundRepeat, BackgroundRepeat)
repeatX = (Repeat, NoRepeat)

repeatY :: (BackgroundRepeat, BackgroundRepeat)
repeatY = (NoRepeat, Repeat)

space :: (BackgroundRepeat, BackgroundRepeat)
space = (Space, Space)

round :: (BackgroundRepeat, BackgroundRepeat)
round = (Round, Round)

noRepeat :: (BackgroundRepeat, BackgroundRepeat)
noRepeat = (NoRepeat, NoRepeat)

backgroundColor :: Color -> Css
backgroundColor = key "background-color"

backgroundPosition :: BackgroundPosition -> Css
backgroundPosition = key "background-position"

backgroundSize :: BackgroundSize -> Css
backgroundSize = key "background-size"

backgroundRepeat :: (BackgroundRepeat, BackgroundRepeat) -> Css
backgroundRepeat = key "background-repeat"

data Gradient = Grad

data BackgroundImage
  = Url Text
  -- | Gradient Gradient

url :: Text -> Maybe BackgroundImage
url = Just . Url

instance Val BackgroundImage where
  value (Url u) = Value ("url(\"" <> u <> "\")")

backgroundImage :: Maybe BackgroundImage -> Css
backgroundImage = key "background-image"

-- backgroundOrigin     = key "background-origin"
-- backgroundClip       = key "background-clip"
-- backgroundAttachment = key "background-attachment"

class Val a => Background a where
  background :: a -> Css
  background = key "background"

instance Background Color
instance Background BackgroundPosition
instance Background BackgroundImage
instance Background BackgroundSize
instance Background (BackgroundPosition, BackgroundPosition)
instance Background BackgroundRepeat
instance Background (BackgroundRepeat, BackgroundRepeat)

-------------------------------------------------------------------------------

data Stroke = Solid | Dotted | Dashed
  deriving Show

solid, dotted, dashed :: Stroke
solid  = Solid
dotted = Dotted
dashed = Dashed

instance Val Stroke where
  value Solid  = "solid"
  value Dotted = "dotted"
  value Dashed = "dashed"

border, borderTop, borderLeft, borderBottom, borderRight :: Stroke -> Size -> Color -> Css

border       = key3 "border"
borderTop    = key3 "border-top"
borderLeft   = key3 "border-left"
borderBottom = key3 "border-bottom"
borderRight  = key3 "border-right"

borderLeftColor, borderRightColor, borderTopColor, borderBottomColor :: Color -> Css

borderLeftColor   = key "border-left-color"
borderRightColor  = key "border-right-color"
borderTopColor    = key "border-top-color"
borderBottomColor = key "border-bottom-color"

borderLeftStyle, borderRightStyle, borderTopStyle, borderBottomStyle :: Stroke -> Css

borderLeftStyle   = key "border-left-style"
borderRightStyle  = key "border-right-style"
borderTopStyle    = key "border-top-style"
borderBottomStyle = key "border-bottom-style"

borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomWidth :: Size -> Css

borderLeftWidth   = key "border-left-width"
borderRightWidth  = key "border-right-width"
borderTopWidth    = key "border-top-width"
borderBottomWidth = key "border-bottom-width"

