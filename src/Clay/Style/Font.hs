{-# LANGUAGE OverloadedStrings #-}
module Clay.Style.Font where

import Data.Text (Text)
import Prelude hiding (Left, Right)

import Clay.Core.Property
import Clay.Core.Rule
import Clay.Style.Color
import Clay.Style.Size

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

