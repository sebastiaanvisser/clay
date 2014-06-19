{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Clay.List
( listStyleType
, disc
, decimal
, hiragana
)
where

import Clay.Common
import Clay.Property
import Clay.Stylesheet

newtype ListStyleType = ListStyleType Value
  deriving (Val, Auto, Inherit, Other)

disc, decimal, hiragana  :: ListStyleType

disc        = ListStyleType "disc"
decimal     = ListStyleType "decimal"
hiragana    = ListStyleType "hiragana"

listStyleType :: ListStyleType -> Css
listStyleType = key "list-style-type"
