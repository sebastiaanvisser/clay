{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  , FlexibleInstances
  #-}
module Clay.Font
(

-- * Generic font property.

  Font (font)
, Optional (..)
, Required (..)

-- * Color.

, fontColor
, color

-- * Font-family.

, fontFamily
, sansSerif
, serif
, monospace

-- * Font-size.

, FontSize
, xxSmall, xSmall, small, medium, large, xLarge, xxLarge, smaller, larger
, fontSize
, fontSizeCustom

-- * Font-style

, FontStyle
, italic, oblique
, fontStyle

-- * Font-variant.

, FontVariant
, smallCaps
, fontVariant

-- * Font-weight

, FontWeight
, bold, bolder, lighter
, weight
, fontWeight

-- * Named fonts.

, NamedFont
, caption, icon, menu, messageBox, smallCaption, statusBar

-- * Line-height.

, lineHeight
)
where

import Data.Text (pack)
import Data.Monoid
import Prelude hiding (Left, Right)

import Clay.Color
import Clay.Common
import Clay.Property
import Clay.Stylesheet
import Clay.Size

-- | We implement the generic font property as a type class that accepts
-- multiple value types. This allows us to combine different font aspects into
-- a shorthand syntax. Fonts require a mandatory part and have a optional a
-- part.

class Val a => Font a where
  font :: a -> Css
  font = key "font"

data Optional =
  Optional
  (Maybe FontWeight)
  (Maybe FontVariant)
  (Maybe FontStyle)

instance Val Optional where
  value (Optional a b c) = value (a ! b ! c)

data Required a =
  Required
  (Size a)
  (Maybe (Size a))
  [Literal]

instance Val (Required a) where
  value (Required a Nothing  c) = value (a ! c)
  value (Required a (Just b) c) = value ((value a <> "/" <> value b) ! c)

instance Font (          Required a)
instance Font (Optional, Required a)

-------------------------------------------------------------------------------

-- | An alias for color.

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

