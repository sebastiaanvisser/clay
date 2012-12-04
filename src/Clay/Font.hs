{-# LANGUAGE OverloadedStrings #-}
module Clay.Font where

import Data.Text (Text)
import Prelude hiding (Left, Right)

import Clay.Property
import Clay.Rule
import Clay.Color
import Clay.Size

font :: Text -> Size a -> Color -> Css
font a b c = key "font" (a ! b ! c)

fontFamily :: Text -> Css
fontFamily = key "font-family"

fontSize :: Size a -> Css
fontSize = key "font-family"

color :: Color -> Css
color = key "color"

fontColor :: Color -> Css
fontColor = key "color"

