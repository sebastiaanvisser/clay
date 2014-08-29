{-# LANGUAGE OverloadedStrings #-}
module Clay.Geometry
(
-- * Positioning.
  size, top, left, bottom, right

-- * Sizing.
, width, height, minWidth, minHeight, maxWidth, maxHeight

-- * Padding.
, padding
, paddingTop, paddingLeft, paddingRight, paddingBottom

-- * Margin.
, margin
, marginTop, marginLeft, marginRight, marginBottom
)
where

import Clay.Property
import Clay.Stylesheet
import Clay.Size

-------------------------------------------------------------------------------

size, top, left, bottom, right :: Size a -> Css

size      = key "size"
top       = key "top"
left      = key "left"
bottom    = key "bottom"
right     = key "right"

width, height, minWidth, minHeight, maxWidth, maxHeight :: Size a -> Css

width     = key "width"
height    = key "height"
minWidth  = key "min-width"
minHeight = key "min-height"
maxWidth  = key "max-width"
maxHeight = key "max-height"

-------------------------------------------------------------------------------

padding :: Size a -> Size a -> Size a -> Size a -> Css
padding a b c d = key "padding" (a ! b ! c ! d)

paddingTop, paddingLeft, paddingRight, paddingBottom :: Size a -> Css

paddingTop    = key "padding-top"
paddingLeft   = key "padding-left"
paddingRight  = key "padding-right"
paddingBottom = key "padding-bottom"

-------------------------------------------------------------------------------

margin :: Size a -> Size a -> Size a -> Size a -> Css
margin a b c d = key "margin"  (a ! b ! c ! d)

marginTop, marginLeft, marginRight, marginBottom :: Size a -> Css

marginTop     = key "margin-top"
marginLeft    = key "margin-left"
marginRight   = key "margin-right"
marginBottom  = key "margin-bottom"

