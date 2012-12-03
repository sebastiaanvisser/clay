{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  , FlexibleInstances
  #-}
module Clay.Style.Size where

import Data.Monoid
import Data.Text (pack)

import Clay.Core.Property

data Rel
data Abs

newtype Size a = Size Value
  deriving Val

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

