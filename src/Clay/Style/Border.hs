{-# LANGUAGE OverloadedStrings #-}
module Clay.Style.Border where

import Clay.Core.Property
import Clay.Core.Rule
import Clay.Style.Color
import Clay.Style.Size

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

border, borderTop, borderLeft, borderBottom, borderRight :: Stroke -> Size Abs -> Color -> Css

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

borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomWidth :: Size Abs -> Css

borderLeftWidth   = key "border-left-width"
borderRightWidth  = key "border-right-width"
borderTopWidth    = key "border-top-width"
borderBottomWidth = key "border-bottom-width"

