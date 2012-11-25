{-# LANGUAGE OverloadedStrings #-}
module Style where

import Data.Text (Text)

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

padding_top, padding_left, padding_right, padding_bottom,
  margin_top, margin_left, margin_right, margin_bottom :: Size -> Css

padding_top    = key "paddingTop"
padding_left   = key "paddingLeft"
padding_right  = key "paddingRight"
padding_bottom = key "paddingBottom"
margin_top     = key "marginTop"
margin_left    = key "marginLeft"
margin_right   = key "marginRight"
margin_bottom  = key "marginBottom"

sym :: (Size -> Size -> Size -> Size -> Css) -> Size -> Size -> Css
sym k a b = k a b a b

-------------------------------------------------------------------------------

font :: Text -> Size -> Color -> Css
font = key3 "font"

font_family :: Text -> Css
font_family = key "font-family"

font_size :: Size -> Css
font_size = key "font-family"

color :: Color -> Css
color = key "color"

font_color :: Color -> Css
font_color = key "color"

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

border, border_top, border_left, border_bottom, border_right :: Stroke -> Size -> Color -> Css

border        = key3 "border"
border_top    = key3 "border-top"
border_left   = key3 "border-left"
border_bottom = key3 "border-bottom"
border_right  = key3 "border-right"

border_left_color, border_right_color, border_top_color, border_bottom_color :: Color -> Css

border_left_color   = key "border-left-color"
border_right_color  = key "border-right-color"
border_top_color    = key "border-top-color"
border_bottom_color = key "border-bottom-color"

border_left_style, border_right_style, border_top_style, border_bottom_style :: Stroke -> Css

border_left_style   = key "border-left-style"
border_right_style  = key "border-right-style"
border_top_style    = key "border-top-style"
border_bottom_style = key "border-bottom-style"

border_left_width, border_right_width, border_top_width, border_bottom_width :: Size -> Css

border_left_width   = key "border-left-width"
border_right_width  = key "border-right-width"
border_top_width    = key "border-top-width"
border_bottom_width = key "border-bottom-width"

