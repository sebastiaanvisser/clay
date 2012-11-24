{-# LANGUAGE OverloadedStrings #-}
module Style where

import Data.Char
import qualified Data.Text as Text

import Color
import Size
import Rules

data Stroke = Solid | Dotted | Dashed
  deriving Show

solid, dotted, dashed :: Stroke
solid  = Solid
dotted = Dotted
dashed = Dashed

border :: Stroke -> Size -> Color -> Prop
border b s c = "border" -: Text.intercalate " "
  [ Text.pack (map toLower (show b))
  , Size.render s
  , Color.render c
  ]

