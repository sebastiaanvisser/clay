{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Clay.Geometry
(
-- * Positioning.
  size, top, left, bottom, right

-- * Sizing.
, width, height, minWidth, minHeight, maxWidth, maxHeight

-- ** Aspect ratio.
, AspectRatio
, aspectRatio
, (%)
, withFallback

-- * Padding.
, padding
, paddingTop, paddingLeft, paddingRight, paddingBottom

-- * Margin.
, margin
, marginTop, marginLeft, marginRight, marginBottom
)
where

import qualified Data.Ratio as R
import Data.String (fromString)
import Clay.Common
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

data AspectRatio = AspectRatio Rational
                 | AspectRatioValue Value
                 | AspectRatioWithFallback (AspectRatio, AspectRatio)

instance Auto AspectRatio where auto = AspectRatioValue auto
instance Inherit AspectRatio where inherit = AspectRatioValue inherit
instance Initial AspectRatio where initial = AspectRatioValue initial
instance Unset AspectRatio where unset = AspectRatioValue unset

instance Num AspectRatio where
  fromInteger = AspectRatio . toRational
  (+)    = error   "plus not implemented for AspectRatio"
  (*)    = error  "times not implemented for AspectRatio"
  abs    = error    "abs not implemented for AspectRatio"
  signum = error "signum not implemented for AspectRatio"
  negate = error "negate not implemented for AspectRatio"

instance Fractional AspectRatio where
  fromRational = AspectRatio
  recip  = error  "recip not implemented for AspectRatio"

instance Val AspectRatio where
  value (AspectRatioValue v) = v
  value (AspectRatio r) = v
    where v = fromString $ numerator <> "/" <> denominator :: Value
          numerator = show (R.numerator r)
          denominator = show (R.denominator r)
  value (AspectRatioWithFallback (a, b)) = value a <> " " <> value b

aspectRatio :: AspectRatio -> Css
aspectRatio = key "aspect-ratio"

(%) :: Integer -> Integer -> AspectRatio
(%) m n = fromRational $ (R.%) m n

infixl 7  %

class WithFallback a where
  withFallback :: a -> a -> a

instance WithFallback AspectRatio where
  withFallback x@(AspectRatioValue "auto") y@(AspectRatio _) =
    AspectRatioWithFallback (x, y)
  withFallback x@(AspectRatio _) y@(AspectRatioValue "auto") =
    AspectRatioWithFallback (x, y)
  withFallback _ _ =
    error "aspectRatio withFallback must be auto and a ratio in either order"

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
