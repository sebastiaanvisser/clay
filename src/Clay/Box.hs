{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Clay.Box
( BoxType
, paddingBox, borderBox, contentBox
, boxSizing
, boxShadow
, boxShadows
, insetBoxShadow
)
where

import Data.Monoid

import Clay.Color
import Clay.Common
import Clay.Property
import Clay.Stylesheet
import Clay.Size
import Clay.Border

-------------------------------------------------------------------------------

newtype BoxType = BoxType Value
  deriving (Val, Inherit)

paddingBox, borderBox, contentBox :: BoxType

paddingBox = BoxType "padding-box"
borderBox  = BoxType "border-box"
contentBox = BoxType "content-box"

-------------------------------------------------------------------------------

boxSizing :: BoxType -> Css
boxSizing = prefixed (browsers <> "box-sizing")

-------------------------------------------------------------------------------

boxShadow :: Size a -> Size a -> Size a -> Color -> Css
boxShadow x y w c = prefixed (browsers <> "box-shadow") (x ! y ! w ! c)

boxShadows :: [(Size a, Size a, Size a, Color)] -> Css
boxShadows = prefixed (browsers <> "box-shadow") . map (\(a, b, c, d) -> a ! b ! c ! d)

-------------------------------------------------------------------------------

insetBoxShadow :: Stroke -> Size a -> Size a -> Size a -> Color -> Css
insetBoxShadow x y w c z = prefixed (browsers <> "box-shadow") (x ! y ! w ! c ! z)
