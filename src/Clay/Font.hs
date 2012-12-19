{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Clay.Font where

import Data.Text (pack)
import Data.Monoid
import Prelude hiding (Left, Right)

import Clay.Color
import Clay.Common
import Clay.Property
import Clay.Stylesheet
import Clay.Size

-- Background property as a type class.

class Val a => Font a where
  font :: a -> Css
  font = key "font"

data FontOptional = FontOptional (Maybe FontWeight) (Maybe FontVariant) (Maybe FontStyle)

instance Val FontOptional where
  value (FontOptional a b c) = value (a ! b ! c)

data FontMandatory a = FontMandatory (Size a) (Maybe (Size a)) [Literal]

instance Val (FontMandatory a) where
  value (FontMandatory a Nothing  c) = value (a ! c)
  value (FontMandatory a (Just b) c) = value ((value a <> "/" <> value b) ! c)

instance Font (              FontMandatory a)
instance Font (FontOptional, FontMandatory a)

-------------------------------------------------------------------------------

fontColor :: Color -> Css
fontColor = key "color"

color :: Color -> Css
color = key "color"

-------------------------------------------------------------------------------

fontFamily :: [Literal] -> Css
fontFamily = key "font-family"

sansSerif :: Literal
sansSerif = "sans-serif"

serif :: Literal
serif = "serif"

monospace :: Literal
monospace = "fixed"

-------------------------------------------------------------------------------

newtype FontSize = FontSize Value
  deriving (Val, Inherit, Auto)

xxSmall, xSmall, small, medium, large, xLarge, xxLarge, smaller, larger :: FontSize

xxSmall = FontSize "xx-small"
xSmall  = FontSize "x-small"
small   = FontSize "small"
medium  = FontSize "medium"
large   = FontSize "large"
xLarge  = FontSize "x-large"
xxLarge = FontSize "xx-large"
smaller = FontSize "smaller"
larger  = FontSize "larger"

fontSize :: Size a -> Css
fontSize = key "font-size"

fontSizeCustom :: FontSize -> Css
fontSizeCustom = key "font-size"

-------------------------------------------------------------------------------

newtype FontStyle = FontStyle Value
  deriving (Val, Inherit, Normal)

italic, oblique :: FontStyle

italic = FontStyle "italic"
oblique = FontStyle "oblique"

fontStyle :: FontStyle -> Css
fontStyle = key "font-style"

-------------------------------------------------------------------------------

newtype FontVariant = FontVariant Value
  deriving (Val, Inherit, Normal)

smallCaps :: FontVariant
smallCaps = FontVariant "small-caps"

fontVariant :: FontVariant -> Css
fontVariant = key "font-variant"

-------------------------------------------------------------------------------

newtype FontWeight = FontWeight Value
  deriving (Val, Inherit, Normal)

bold, bolder, lighter :: FontWeight

bold    = FontWeight "bold"
bolder  = FontWeight "bolder"
lighter = FontWeight "lighter"

weight :: Integer -> FontWeight
weight i = FontWeight (value (pack (show i)))

fontWeight :: FontWeight -> Css
fontWeight = key "font-weight"

-------------------------------------------------------------------------------

newtype NamedFont = NamedFont Value
  deriving Val

caption, icon, menu, messageBox, smallCaption, statusBar :: NamedFont

caption      = NamedFont "caption"
icon         = NamedFont "icon"
menu         = NamedFont "menu"
messageBox   = NamedFont "message-box"
smallCaption = NamedFont "small-caption"
statusBar    = NamedFont "status-bar"

-------------------------------------------------------------------------------

lineHeight :: Size a -> Css
lineHeight = key "line-height"

