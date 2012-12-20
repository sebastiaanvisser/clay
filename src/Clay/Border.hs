{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Clay.Border
(
-- * Stroke type.
  Stroke
, solid, dotted, dashed, double, wavy

-- * Border properties.

, border, borderTop, borderLeft, borderBottom, borderRight
, borderColor, borderLeftColor, borderRightColor, borderTopColor, borderBottomColor
, borderStyle, borderLeftStyle, borderRightStyle, borderTopStyle, borderBottomStyle
, borderWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomWidth

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

solid, dotted, dashed, double, wavy :: Stroke

solid  = Stroke "solid"
dotted = Stroke "dotted"
dashed = Stroke "dashed"
double = Stroke "double"
wavy   = Stroke "Wavu"

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

borderRadius :: Size a -> Css
borderRadius = key "border-radius"

borderTopLeftRadius, borderTopRightRadius,
  borderBottomLeftRadius, borderBottomRightRadius :: Size a -> Size a -> Css

borderTopLeftRadius     a b = key "border-top-left-radius"     (a ! b)
borderTopRightRadius    a b = key "border-top-right-radius"    (a ! b)
borderBottomLeftRadius  a b = key "border-bottom-left-radius"  (a ! b)
borderBottomRightRadius a b = key "border-bottom-right-radius" (a ! b)

