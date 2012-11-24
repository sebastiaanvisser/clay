{-# LANGUAGE OverloadedStrings #-}
module Size
( Size
, px
, pt
, pct
, em

, render
)
where

import Data.Monoid
import Data.Text (Text, pack)

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

render :: Size -> Text
render s =
  case s of
    Px  i -> p (round i :: Integer) <> "px"
    Pt  i -> p i                    <> "pt"
    Pct i -> p i                    <> "pct"
    Em  i -> p i                    <> "em"
  where p :: Show a => a -> Text
        p = pack . show

