{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Clay.Border where

import Clay.Property
import Clay.Stylesheet
import Clay.Color
import Clay.Common
import Clay.Size

newtype Stroke = Stroke Value
  deriving Val

instance Auto    Stroke where auto    = Stroke "auto"
instance Inherit Stroke where inherit = Stroke "inherit"
instance None    Stroke where none    = Stroke "none"
instance Other   Stroke where other   = Stroke

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

borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomWidth :: Size Abs -> Css

borderLeftWidth   = key "border-left-width"
borderRightWidth  = key "border-right-width"
borderTopWidth    = key "border-top-width"
borderBottomWidth = key "border-bottom-width"

-------------------------------------------------------------------------------

borderRadius :: Size Abs -> Css
borderRadius = key "border-radius"

borderTopLeftRadius, borderTopRightRadius,
  borderBottomLeftRadius, borderBottomRightRadius :: Size a -> Size a -> Css

borderTopLeftRadius     a b = key "border-top-left-radius"     (a ! b)
borderTopRightRadius    a b = key "border-top-right-radius"    (a ! b)
borderBottomLeftRadius  a b = key "border-bottom-left-radius"  (a ! b)
borderBottomRightRadius a b = key "border-bottom-right-radius" (a ! b)

