{-# LANGUAGE OverloadedStrings #-}
module Clay.Style.Size
( Size
, px
, pt
, pct
, em
)
where

import Data.Monoid
import Data.Text (Text, pack)

import Clay.Core.Property

data Size
  = Px  Double
  | Pt  Double
  | Pct Double
  | Em  Double

px, pt, pct, em :: Double -> Size
px  = Px
pt  = Pt
pct = Pct
em  = Em

instance Val Size where
  value s = Value $
    case s of
      Px  i -> p (round i :: Integer) <> "px"
      Pt  i -> p i                    <> "pt"
      Pct i -> p i                    <> "%"
      Em  i -> p i                    <> "em"
    where p :: Show a => a -> Text
          p = pack . show

