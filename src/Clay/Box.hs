{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, CPP #-}
module Clay.Box
( BoxType
, paddingBox, borderBox, contentBox
, boxSizing
-- * @box-shadow@
-- $box-shadow
, boxShadow
, shadow
, shadowWithBlur
, shadowWithSpread
, bsInset
, bsColor
-- ** Deprecated
-- $box-shadow-deprecated
, boxShadow'
, boxShadowWithSpread
, boxShadows
, insetBoxShadow
)
where

#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
import Data.List.NonEmpty (NonEmpty)

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

-- $box-shadow
--
-- === Formal argument syntax
--
-- > none | <shadow>#
-- > where 
-- > <shadow> = inset? && <length>{2,4} && <color>?
-- > 
-- > where 
-- > <color> = <rgb()> | <rgba()> | <hsl()> | <hsla()> | <hex-color> | <named-color> | currentcolor | <deprecated-system-color>
-- > 
-- > where 
-- > <rgb()> = rgb( [ [ <percentage>{3} | <number>{3} ] [ / <alpha-value> ]? ] | [ [ <percentage>#{3} | <number>#{3} ] , <alpha-value>? ] )
-- > <rgba()> = rgba( [ [ <percentage>{3} | <number>{3} ] [ / <alpha-value> ]? ] | [ [ <percentage>#{3} | <number>#{3} ] , <alpha-value>? ] )
-- > <hsl()> = hsl( [ <hue> <percentage> <percentage> [ / <alpha-value> ]? ] | [ <hue>, <percentage>, <percentage>, <alpha-value>? ] )
-- > <hsla()> = hsla( [ <hue> <percentage> <percentage> [ / <alpha-value> ]? ] | [ <hue>, <percentage>, <percentage>, <alpha-value>? ] )
-- > 
-- > where 
-- > <alpha-value> = <number> | <percentage>
-- > <hue> = <number> | <angle>

newtype BoxShadow = BoxShadow Value
  deriving (Val, Inherit, Initial, Unset, None, Other)

-- | This function will usually take a singleton list, but requiring a (non-empty)
-- list prevents accidentally applying the modifiers ('bsInset', 'bsColor')
-- incorrectly.
--
-- 'pure' (from "Control.Applicative") creates a singleton list.
--
-- > boxShadow . pure $ none
-- > boxShadow . pure $ shadow (px 1) (px 1)
--
-- Use with @{-# LANGUAGE OverloadedLists #-}@ for the simplest list syntax.
--
-- > boxShadow [none]
-- > boxShadow [shadow (px 1) (px 1)]
--
-- This is recommended for supplying multiple 'BoxShadow' values.
--
-- > boxShadow [shadowWithBlur (em 2) (em 1), bsInset . bsColor red $ shadow (px 1) (px 2)]
boxShadow :: NonEmpty BoxShadow -> Css
boxShadow = prefixed (browsers <> "box-shadow") . value

shadow
  :: Size a -- ^ Horizontal offset
  -> Size a -- ^ Vertical offset
  -> BoxShadow
shadow x y = BoxShadow . value $ (x ! y)

shadowWithBlur
  :: Size a -- ^ Horizontal offset
  -> Size a -- ^ Vertical offset
  -> Size a -- ^ Blur radius
  -> BoxShadow
shadowWithBlur x y w = BoxShadow . value $ (x ! y ! w)

-- | While this function is the correct type to work with the 'sym', 'sym2'
-- or 'sym3' functions, such applications are unlikely to be meaningful.
shadowWithSpread
  :: Size a -- ^ Horizontal offset
  -> Size a -- ^ Vertical offset
  -> Size a -- ^ Blur radius
  -> Size a -- ^ Spread radius
  -> BoxShadow
shadowWithSpread x y blurRadius spreadRadius =
    BoxShadow . value $ (x ! y ! blurRadius ! spreadRadius)

-- | Adapt the provided @box-shadow@ with the @inset@ prefix.
-- 
-- > boxShadow . pure . bsInset
bsInset :: BoxShadow -> BoxShadow
bsInset (BoxShadow v) = BoxShadow . value $ ("inset" :: Value, v)

-- | Supply a color to the provided @box-shadow@.
bsColor :: Color -> BoxShadow -> BoxShadow
bsColor c (BoxShadow v) = BoxShadow . value $ (v, c)
infixr 9 `bsColor`

-------------------------------------------------------------------------------

-- $box-shadow-deprecated
--
-- The old implementation was both restrictive and slightly off-spec. It
-- shall be discontinued in a future release. The 'boxShadow' name has already
-- been taken, so the functionality that used to provide is now provided by
-- 'boxShadow\''.

-- | This is the drop-in replacement for the old 'boxShadow' function (< 0.13).
-- It is possible to continue for now
boxShadow' :: Size a -> Size a -> Size a -> Color -> Css
boxShadow' x y w c = prefixed (browsers <> "box-shadow") (x ! y ! w ! c)
{-# DEPRECATED boxShadow' "This function is only present for compatibility purposes and will be removed." #-}

-- | Replace calls to this function as follows (using -XOverloadedLists)
--
-- > boxShadowWithSpread x y rb rs c
--
-- > boxShadow [c `bsColor` shadowWithSpread x y rb rs]
boxShadowWithSpread :: Size a -> Size a -> Size a -> Size a -> Color -> Css
boxShadowWithSpread x y blurRadius spreadRadius color =
    prefixed (browsers <> "box-shadow") (x ! y ! blurRadius ! spreadRadius ! color)
{-# DEPRECATED boxShadowWithSpread "This function has been replaced with shadowWithSpread and bsColor and will be removed." #-}

-- | Replace calls to this function as follows (using -XOverloadedLists)
--
-- > boxShadows [(x1, y1, rb1, c1), (x2, y2, rb2, c2)]
--
-- > boxShadow
-- >   [ c1 `bsColor` shadowWithBlur x1 y1 rb1
-- >   , c2 `bsColor` shadowWithBlur x2 y2 rb2
-- >   ]
boxShadows :: [(Size a, Size a, Size a, Color)] -> Css
boxShadows = prefixed (browsers <> "box-shadow") . map (\(a, b, c, d) -> a ! b ! c ! d)
{-# DEPRECATED boxShadows "This function is replaced with boxShadow and will be removed." #-}

-- | Replace calls to this function as follows (using -XOverloadedLists)
--
-- > insetBoxShadow s x y rb c
--
-- > boxShadow [bsInset $ c `bsColor` shadowWithBlur x y rb]
insetBoxShadow :: Stroke -> Size a -> Size a -> Size a -> Color -> Css
insetBoxShadow x y w c z = prefixed (browsers <> "box-shadow") (x ! y ! w ! c ! z)
{-# DEPRECATED insetBoxShadow "This function has been replaced with shadowWithSpread, bsInset and bsColor and will be removed." #-}
