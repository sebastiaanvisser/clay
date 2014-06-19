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
, Abs
, Rel
, nil

-- * Size constructors.

, px
, pt
, em
, ex
, pct

-- * Shorthands for properties that can be applied separately to each box side.

, sym
, sym2
, sym3

-- * Angle type.

, Angle
, Deg
, Rad

-- * Constructing angles.

, deg
, rad

)
where

import Data.Monoid

import Clay.Common
import Clay.Property
import Clay.Stylesheet

-------------------------------------------------------------------------------

-- | Sizes can be relative like percentages.
data Rel

-- | Sizes can be absolute like pixels, points, etc.
data Abs

newtype Size a = Size Value
  deriving (Val, Auto, Normal, Inherit, None, Other)

nil :: Size a
nil = Size "0"

-- | Size in pixels.

px :: Integer -> Size Abs
px i = Size (value i <> "px")

-- | Size in points.

pt :: Double -> Size Abs
pt i = Size (value i <> "pt")

-- | Size in em's.

em :: Double -> Size Abs
em i = Size (value i <> "em")

-- | Size in ex'es.

ex :: Double -> Size Abs
ex i = Size (value i <> "ex")

-- | Size in percentages.

pct :: Double -> Size Rel
pct i = Size (value i <> "%")

instance Num (Size Abs) where
  fromInteger = px
  (+)    = error   "plus not implemented for Size"
  (*)    = error  "times not implemented for Size"
  abs    = error    "abs not implemented for Size"
  signum = error "signum not implemented for Size"
  negate = error "negate not implemented for Size"

instance Fractional (Size Abs) where
  fromRational = em . fromRational
  recip  = error  "recip not implemented for Size"

instance Num (Size Rel) where
  fromInteger = pct . fromInteger
  (+)    = error   "plus not implemented for Size"
  (*)    = error  "times not implemented for Size"
  abs    = error    "abs not implemented for Size"
  signum = error "signum not implemented for Size"
  negate = error "negate not implemented for Size"

instance Fractional (Size Rel) where
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

newtype Angle a = Angle Value
  deriving (Val, Auto, Inherit, Other)

-- | Angle in degrees.

deg :: Double -> Angle Deg
deg i = Angle (value i <> "deg")

-- | Angle in radians.

rad :: Double -> Angle Rad
rad i = Angle (value i <> "rad")

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

