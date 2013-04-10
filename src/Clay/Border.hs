{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Clay.Border
(
-- * Stroke type, used for border-style and outline-style.
  Stroke
, solid, dotted, dashed, double, wavy, groove, ridge, inset, outset

-- * Border properties.

, border, borderTop, borderLeft, borderBottom, borderRight
, borderColor, borderLeftColor, borderRightColor, borderTopColor, borderBottomColor
, borderStyle, borderLeftStyle, borderRightStyle, borderTopStyle, borderBottomStyle
, borderWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomWidth

-- * Outline properties.

, outline, outlineTop, outlineLeft, outlineBottom, outlineRight
, outlineColor, outlineLeftColor, outlineRightColor, outlineTopColor, outlineBottomColor
, outlineStyle, outlineLeftStyle, outlineRightStyle, outlineTopStyle, outlineBottomStyle
, outlineWidth, outlineLeftWidth, outlineRightWidth, outlineTopWidth, outlineBottomWidth
, outlineOffset

-- * Border radius.

, borderRadius
, borderTopLeftRadius, borderTopRightRadius
, borderBottomLeftRadius, borderBottomRightRadius
)
where

import Clay.Property
import Clay.Stylesheet
import Clay.Color
import Clay.Common
import Clay.Size

newtype Stroke = Stroke Value
  deriving (Val, Other, Inherit, Auto, None)

solid, dotted, dashed, double, wavy, groove, ridge, inset, outset :: Stroke

solid  = Stroke "solid"
dotted = Stroke "dotted"
dashed = Stroke "dashed"
double = Stroke "double"
wavy   = Stroke "wavy"
groove = Stroke "groove"
ridge  = Stroke "ridge"
inset  = Stroke "inset"
outset = Stroke "outset"

-------------------------------------------------------------------------------

border, borderTop, borderLeft, borderBottom, borderRight :: Stroke -> Size Abs -> Color -> Css

border        a b c = key "border"        (a ! b ! c)
borderTop     a b c = key "border-top"    (a ! b ! c)
borderLeft    a b c = key "border-left"   (a ! b ! c)
borderBottom  a b c = key "border-bottom" (a ! b ! c)
borderRight   a b c = key "border-right"  (a ! b ! c)

borderColor, borderLeftColor, borderRightColor, borderTopColor, borderBottomColor :: Color -> Css

borderColor       = key "border-color"
borderLeftColor   = key "border-left-color"
borderRightColor  = key "border-right-color"
borderTopColor    = key "border-top-color"
borderBottomColor = key "border-bottom-color"

borderStyle, borderLeftStyle, borderRightStyle, borderTopStyle, borderBottomStyle :: Stroke -> Css

borderStyle       = key "border-style"
borderLeftStyle   = key "border-left-style"
borderRightStyle  = key "border-right-style"
borderTopStyle    = key "border-top-style"
borderBottomStyle = key "border-bottom-style"

borderWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomWidth :: Size Abs -> Css

borderWidth       = key "border-width"
borderLeftWidth   = key "border-left-width"
borderRightWidth  = key "border-right-width"
borderTopWidth    = key "border-top-width"
borderBottomWidth = key "border-bottom-width"

-------------------------------------------------------------------------------

outline, outlineTop, outlineLeft, outlineBottom, outlineRight :: Stroke -> Size Abs -> Color -> Css

outline        a b c = key "outline"        (a ! b ! c)
outlineTop     a b c = key "outline-top"    (a ! b ! c)
outlineLeft    a b c = key "outline-left"   (a ! b ! c)
outlineBottom  a b c = key "outline-bottom" (a ! b ! c)
outlineRight   a b c = key "outline-right"  (a ! b ! c)

outlineColor, outlineLeftColor, outlineRightColor, outlineTopColor, outlineBottomColor :: Color -> Css

outlineColor       = key "outline-color"
outlineLeftColor   = key "outline-left-color"
outlineRightColor  = key "outline-right-color"
outlineTopColor    = key "outline-top-color"
outlineBottomColor = key "outline-bottom-color"

outlineStyle, outlineLeftStyle, outlineRightStyle, outlineTopStyle, outlineBottomStyle :: Stroke -> Css

outlineStyle       = key "outline-style"
outlineLeftStyle   = key "outline-left-style"
outlineRightStyle  = key "outline-right-style"
outlineTopStyle    = key "outline-top-style"
outlineBottomStyle = key "outline-bottom-style"

outlineWidth, outlineLeftWidth, outlineRightWidth, outlineTopWidth, outlineBottomWidth :: Size Abs -> Css

outlineWidth       = key "outline-width"
outlineLeftWidth   = key "outline-left-width"
outlineRightWidth  = key "outline-right-width"
outlineTopWidth    = key "outline-top-width"
outlineBottomWidth = key "outline-bottom-width"

outlineOffset :: Size Abs -> Css
outlineOffset = key "outline-offset"

-------------------------------------------------------------------------------

borderRadius :: Size a -> Size a -> Size a -> Size a -> Css
borderRadius a b c d = key "border-radius" (a ! b ! c ! d)

borderTopLeftRadius, borderTopRightRadius,
  borderBottomLeftRadius, borderBottomRightRadius :: Size a -> Size a -> Css

borderTopLeftRadius     a b = key "border-top-left-radius"     (a ! b)
borderTopRightRadius    a b = key "border-top-right-radius"    (a ! b)
borderBottomLeftRadius  a b = key "border-bottom-left-radius"  (a ! b)
borderBottomRightRadius a b = key "border-bottom-right-radius" (a ! b)

