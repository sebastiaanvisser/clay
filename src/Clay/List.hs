{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Clay.List
( ListStyleType
, listStyleType
, disc
, decimal
, hiragana

, ListStylePosition
, listStylePosition
, outside

, ListStyleImage
, listStyleImage

, listStyle
)
where

import Clay.Common
import Clay.Property
import Clay.Stylesheet

newtype ListStyleType = ListStyleType Value
  deriving (Val, Initial, Inherit, None, Other)

disc, decimal, hiragana  :: ListStyleType

disc                = ListStyleType "disc"
armenian            = ListStyleType "armenian"
circle              = ListStyleType "circle"
cjkIdeographic      = ListStyleType "cjk-ideographic"
decimal             = ListStyleType "decimal"
decimalLeadingZero  = ListStyleType "decimal-leading-zero"
georgian            = ListStyleType "georgian"
hebrew              = ListStyleType "hebrew"
hiragana            = ListStyleType "hiragana"
hiraganaIroha       = ListStyleType "hiragana-iroha"
katakana            = ListStyleType "katakana"
katakanaIroha       = ListStyleType "katakana-iroha"
lowerAlpha          = ListStyleType "lower-alpha"
lowerGreek          = ListStyleType "lower-greek"
lowerLatin          = ListStyleType "lower-latin"
lowerRoman          = ListStyleType "lower-roman"
square              = ListStyleType "square"
upperAlpha          = ListStyleType "upper-alpha"
upperLatin          = ListStyleType "upper-latin"
upperRoman          = ListStyleType "upper-roman"

listStyleType :: ListStyleType -> Css
listStyleType = key "list-style-type"

newtype ListStylePosition = ListStylePosition Value
  deriving (Val, Initial, Inherit, None, Other)

outside :: ListStylePosition

outside = ListStylePosition "outside"

listStylePosition :: ListStylePosition -> Css
listStylePosition = key "list-style-position"

newtype ListStyleImage = ListStyleImage Value
  deriving (Val, Initial, Inherit, None, Other)

listStyleImage :: ListStyleImage -> Css
listStyleImage = key "list-style-image"

listStyle :: ListStyleType -> ListStylePosition -> ListStyleImage -> Css
listStyle a b c = key "list-style" (a ! b ! c)
