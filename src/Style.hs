{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Style where

import Data.Text (Text)
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

