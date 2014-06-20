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

disc        = ListStyleType "disc"
decimal     = ListStyleType "decimal"
hiragana    = ListStyleType "hiragana"

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
