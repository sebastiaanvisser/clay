{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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

-- | Represents an aspect ratio for use with 'aspectRatio'.
--
-- A fixed ratio can be formed from two integers:
--
-- >>> let _ = 4%3 :: AspectRatio
--
-- An aspect ratio can also be converted from a 'Rational':
--
-- >>> let _ = fromRational 0.5 :: AspectRatio
--
data AspectRatio = AspectRatio Rational
                 | AspectRatioValue Value
                 | AspectRatioWithFallback (AspectRatio, AspectRatio)

instance Auto AspectRatio where auto = AspectRatioValue auto
instance Inherit AspectRatio where inherit = AspectRatioValue inherit
instance Initial AspectRatio where initial = AspectRatioValue initial
instance Unset AspectRatio where unset = AspectRatioValue unset
instance Other AspectRatio where other = AspectRatioValue

-- | An 'AspectRatio' can be converted from an 'Integer',
-- but other operations are not supported.
instance Num AspectRatio where
  fromInteger = AspectRatio . toRational
  (+)    = error   "plus not implemented for AspectRatio"
  (*)    = error  "times not implemented for AspectRatio"
  abs    = error    "abs not implemented for AspectRatio"
  signum = error "signum not implemented for AspectRatio"
  negate = error "negate not implemented for AspectRatio"

-- | An 'AspectRatio' can be converted from a 'Rational',
-- but other operations are not supported.
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

-- | Defines the width to height ratio of an element.
-- At least one of the width or height must be of automatic size,
-- otherwise the aspect ratio will be ignored.
--
-- It can be given a fixed ratio of the width and to the height:
--
-- >>> renderWith compact [] $ aspectRatio (4%3)
-- "{aspect-ratio:4/3}"
--
-- It can also be the intrinsic aspect ratio for the element:
--
-- >>> renderWith compact [] $ aspectRatio auto
-- "{aspect-ratio:auto}"
--
-- It can be told to use the intrinsic aspect ratio for the element,
-- but to use a fixed ratio while it is unknown or does not have one:
--
-- >>> renderWith compact [] $ aspectRatio $ auto `withFallback` (4%3)
-- "{aspect-ratio:auto 4/3}"
--
-- Corresponds to the
-- [@aspect-ratio@](https://developer.mozilla.org/en-US/docs/Web/CSS/aspect-ratio)
-- property in CSS.
aspectRatio :: AspectRatio -> Css
aspectRatio = key "aspect-ratio"

-- | The aspect ratio of the width to the height for use with 'aspectRatio'.
--
-- Note that this is /not/ the same @%@ operator from the "Data.Ratio" module,
-- although they do both semantically represent ratios.  The same symbol is used
-- to signify that the return value is a ratio.
(%) :: Integer -> Integer -> AspectRatio
(%) m n = fromRational $ (R.%) m n

-- The same as the normal % operator.
infixl 7 %

-- | A type class for which a type can have a value with another value as a fallback.
-- Basically, a type class for types which can use 'withFallback'.
--
-- 'withFallback' was defined for 'AspectRatio', but this is a type class
-- because 'withFallback' is a generic name which we may want to reuse
-- for other types in the future.
class WithFallback a where
  -- | Returns a value where one value has another value as a fallback.
  --
  -- * For 'AspectRatio', it can be used to specify that the intrinsic aspect
  --   ratio should be used, but a fixed ratio can be used as a fallback.
  withFallback :: a -> a -> a

instance WithFallback AspectRatio where
  withFallback x@(AspectRatioValue "auto") y@(AspectRatio _) =
    AspectRatioWithFallback (x, y)
  withFallback x@(AspectRatio _) y@(AspectRatioValue "auto") =
    AspectRatioWithFallback (x, y)
  withFallback _ _ =
    error "Arguments for aspectRatio . withFallback must be auto and a ratio in either order"

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
