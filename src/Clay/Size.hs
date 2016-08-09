{-# LANGUAGE
    EmptyDataDecls
  , OverloadedStrings
  , GeneralizedNewtypeDeriving
  , FlexibleInstances
  #-}
module Clay.Size
(

-- * Size type.
  Size
, LengthUnit
, Percentage
, nil
, unitless

-- * Size constructors.

, cm
, mm
, inches
, px
, pt
, pc
, em
, ex
, pct
, rem
, vw
, vh
, vmin
, vmax

-- * Shorthands for properties that can be applied separately to each box side.

, sym
, sym2
, sym3

-- * Angle type.

, Angle
, Deg
, Rad
, Grad
, Turn

-- * Constructing angles.

, deg
, rad
, grad
, turn

)
where

import Data.Monoid
import Prelude hiding (rem)

import Clay.Common
import Clay.Property
import Clay.Stylesheet

-------------------------------------------------------------------------------

-- | Sizes can be given using a length unit (e.g. em, px).
data LengthUnit

-- | Sizes can be given in percentages.
data Percentage

newtype Size a = Size Value
  deriving (Val, Auto, Normal, Inherit, None, Other)

-- | Zero size.
nil :: Size a
nil = Size "0"

-- | Unitless size (as recommended for line-height).
unitless :: Double -> Size a
unitless i = Size (value i)

cm, mm, inches, px, pt, pc :: Double -> Size LengthUnit

-- | Size in centimeters.
cm i = Size (value i <> "cm")

-- | Size in millimeters.
mm i = Size (value i <> "mm")

-- | Size in inches (1in = 2.54 cm).
inches i = Size (value i <> "in")

-- | Size in pixels.
px i = Size (value i <> "px")

-- | Size in points (1pt = 1/72 of 1in).
pt i = Size (value i <> "pt")

-- | Size in picas (1pc = 12pt).
pc i = Size (value i <> "pc")

em, ex, rem, vw, vh, vmin, vmax :: Double -> Size LengthUnit

-- | Size in em's (computed value of the font-size).
em i = Size (value i <> "em")

-- | Size in ex'es (x-height of the first avaliable font).
ex i = Size (value i <> "ex")

-- | Size in rem's (em's, but always relative to the root element).
rem i = Size (value i <> "rem")

-- | Size in vw's (1vw = 1% of viewport width).
vw i = Size (value i <> "vw")

-- | Size in vh's (1vh = 1% of viewport height).
vh i = Size (value i <> "vh")

-- | Size in vmin's (the smaller of vw or vh).
vmin i = Size (value i <> "vmin")

-- | Size in vmax's (the larger of vw or vh).
vmax i = Size (value i <> "vmax")

-- | Size in percents.
pct :: Double -> Size Percentage
pct i = Size (value i <> "%")

instance Num (Size LengthUnit) where
  fromInteger = px . fromInteger
  (+)    = error   "plus not implemented for Size"
  (*)    = error  "times not implemented for Size"
  abs    = error    "abs not implemented for Size"
  signum = error "signum not implemented for Size"
  negate = error "negate not implemented for Size"

instance Fractional (Size LengthUnit) where
  fromRational = px . fromRational
  recip  = error  "recip not implemented for Size"

instance Num (Size Percentage) where
  fromInteger = pct . fromInteger
  (+)    = error   "plus not implemented for Size"
  (*)    = error  "times not implemented for Size"
  abs    = error    "abs not implemented for Size"
  signum = error "signum not implemented for Size"
  negate = error "negate not implemented for Size"

instance Fractional (Size Percentage) where
  fromRational = pct . fromRational
  recip  = error  "recip not implemented for Size"

-------------------------------------------------------------------------------

sym :: (a -> a -> a -> a -> Css) -> a -> Css
sym k a = k a a a a

sym3 :: (a -> a -> a -> a -> Css) -> a -> a -> a -> Css
sym3 k tb l r = k tb l tb r

sym2 :: (a -> a -> a -> a -> Css) -> a -> a -> Css
sym2 k tb lr = k tb lr tb lr

-------------------------------------------------------------------------------

data Deg
data Rad
data Grad
data Turn

newtype Angle a = Angle Value
  deriving (Val, Auto, Inherit, Other)

-- | Angle in degrees.
deg :: Double -> Angle Deg
deg i = Angle (value i <> "deg")

-- | Angle in radians.
rad :: Double -> Angle Rad
rad i = Angle (value i <> "rad")

-- | Angle in gradians (also knows as gons or grades).
grad :: Double -> Angle Grad
grad i = Angle (value i <> "grad")

-- | Angle in turns.
turn :: Double -> Angle Turn
turn i = Angle (value i <> "turn")

instance Num (Angle Deg) where
  fromInteger = deg . fromInteger
  (+)    = error   "plus not implemented for Angle"
  (*)    = error  "times not implemented for Angle"
  abs    = error    "abs not implemented for Angle"
  signum = error "signum not implemented for Angle"
  negate = error "negate not implemented for Angle"

instance Fractional (Angle Deg) where
  fromRational = deg . fromRational
  recip  = error  "recip not implemented for Angle"

instance Num (Angle Rad) where
  fromInteger = rad . fromInteger
  (+)    = error   "plus not implemented for Angle"
  (*)    = error  "times not implemented for Angle"
  abs    = error    "abs not implemented for Angle"
  signum = error "signum not implemented for Angle"
  negate = error "negate not implemented for Angle"

instance Fractional (Angle Rad) where
  fromRational = rad . fromRational
  recip  = error  "recip not implemented for Angle"

instance Num (Angle Grad) where
  fromInteger = grad . fromInteger
  (+)    = error   "plus not implemented for Angle"
  (*)    = error  "times not implemented for Angle"
  abs    = error    "abs not implemented for Angle"
  signum = error "signum not implemented for Angle"
  negate = error "negate not implemented for Angle"

instance Fractional (Angle Grad) where
  fromRational = grad . fromRational
  recip  = error  "recip not implemented for Angle"

instance Num (Angle Turn) where
  fromInteger = turn . fromInteger
  (+)    = error   "plus not implemented for Angle"
  (*)    = error  "times not implemented for Angle"
  abs    = error    "abs not implemented for Angle"
  signum = error "signum not implemented for Angle"
  negate = error "negate not implemented for Angle"

instance Fractional (Angle Turn) where
  fromRational = turn . fromRational
  recip  = error  "recip not implemented for Angle"
