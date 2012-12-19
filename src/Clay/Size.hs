{-# LANGUAGE
    EmptyDataDecls
  , OverloadedStrings
  , GeneralizedNewtypeDeriving
  , FlexibleInstances
  #-}
module Clay.Size where

import Data.Monoid
import Data.Text (pack)

import Clay.Common
import Clay.Property
import Clay.Stylesheet

-------------------------------------------------------------------------------

data Rel
data Abs

newtype Size a = Size Value
  deriving (Val, Auto, Normal, Inherit, None, Other)

px :: Integer -> Size Abs
px i = Size (value (pack (show i) <> "px"))

pt :: Double -> Size Abs
pt i = Size (value (pack (show i) <> "pt"))

em :: Double -> Size Abs
em i = Size (value (pack (show i) <> "em"))

ex :: Double -> Size Abs
ex i = Size (value (pack (show i) <> "ex"))

pct :: Double -> Size Rel
pct i = Size (value (pack (show i) <> "%"))

instance Num (Size Abs) where
  fromInteger = px
  (+)    = error   "plus not implemented for Size"
  (*)    = error  "times not implemented for Size"
  abs    = error    "abs not implemented for Size"
  signum = error "signum not implemented for Size"

instance Fractional (Size Abs) where
  fromRational = em . fromRational

instance Num (Size Rel) where
  fromInteger = pct . fromInteger
  (+)    = error   "plus not implemented for Size"
  (*)    = error  "times not implemented for Size"
  abs    = error    "abs not implemented for Size"
  signum = error "signum not implemented for Size"

instance Fractional (Size Rel) where
  fromRational = pct . fromRational

-------------------------------------------------------------------------------

sym :: (Size a -> Size a -> Size a -> Size a -> Css) -> Size a -> Css
sym k a = k a a a a

sym3 :: (Size a -> Size a -> Size a -> Size a -> Css) -> Size a -> Size a -> Size a -> Css
sym3 k tb l r = k tb l tb r

sym2 :: (Size a -> Size a -> Size a -> Size a -> Css) -> Size a -> Size a -> Css
sym2 k tb lr = k tb lr tb lr

-------------------------------------------------------------------------------

data Deg
data Rad

newtype Angle a = Angle Value
  deriving (Val, Auto, Inherit, Other)

deg :: Double -> Angle Deg
deg i = Angle (value (pack (show i) <> "deg"))

rad :: Double -> Angle Rad
rad i = Angle (value (pack (show i) <> "rad"))

instance Num (Angle Deg) where
  fromInteger = deg . fromInteger
  (+)    = error   "plus not implemented for Angle"
  (*)    = error  "times not implemented for Angle"
  abs    = error    "abs not implemented for Angle"
  signum = error "signum not implemented for Angle"

instance Fractional (Angle Deg) where
  fromRational = deg . fromRational

instance Num (Angle Rad) where
  fromInteger = rad . fromInteger
  (+)    = error   "plus not implemented for Angle"
  (*)    = error  "times not implemented for Angle"
  abs    = error    "abs not implemented for Angle"
  signum = error "signum not implemented for Angle"

instance Fractional (Angle Rad) where
  fromRational = rad . fromRational

