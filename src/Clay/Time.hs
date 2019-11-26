{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Clay.Time
(

-- * Time type.
  Time

-- * Time constructors.

, sec
, ms
)
where

import Data.Text (pack)

import Clay.Common
import Clay.Property

-------------------------------------------------------------------------------

newtype Time = Time Value
  deriving (Val, Auto, Normal, Inherit, None, Other)

-- | Time in seconds.

sec :: Double -> Time
sec i = Time (value (pack (show i) <> "s"))

-- | Time in milliseconds.

ms :: Double -> Time
ms i = Time (value (pack (show i) <> "ms"))

instance Num Time where
  fromInteger = sec . fromInteger
  (+)    = error   "plus not implemented for Time"
  (*)    = error  "times not implemented for Time"
  abs    = error    "abs not implemented for Time"
  signum = error "signum not implemented for Time"
  negate = error "negate not implemented for Time"

instance Fractional Time where
  fromRational = sec . fromRational
  recip  = error  "recip not implemented for Time"

