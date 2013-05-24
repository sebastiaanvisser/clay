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
, cursive
, fantasy

-- * Font-size.

, FontSize
, fontSize
, fontSizeCustom
, xxSmall, xSmall, small, medium, large, xLarge, xxLarge, smaller, larger

-- * Font-style

, FontStyle
, fontStyle
, italic, oblique

-- * Font-variant.

, FontVariant
, fontVariant
, smallCaps

-- * Font-weight

, FontWeight
, fontWeight
, bold, bolder, lighter
, weight

-- * Named fonts.

, NamedFont
, caption, icon, menu, messageBox, smallCaption, statusBar

-- * Line-height.

, lineHeight
)
where

import Control.Applicative
import Data.Text (pack, Text)
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
--
-- <http://www.w3.org/TR/css3-fonts/#font-prop>

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
  [Text]
  [GenericFontFamily]

instance Val (Required a) where
  value (Required a Nothing  c d) = value (a ! (Literal <$> c) ! d)
  value (Required a (Just b) c d) = value ((value a <> "/" <> value b) ! (Literal <$> c) ! d)

instance Font (          Required a)
instance Font (Optional, Required a)

-------------------------------------------------------------------------------

-- | An alias for color.

fontColor :: Color -> Css
fontColor = key "color"

color :: Color -> Css
color = key "color"

-------------------------------------------------------------------------------

-- | The five generic font families.
--
-- <http://www.w3.org/TR/css3-fonts/#generic-font-families>.

newtype GenericFontFamily = GenericFontFamily Value
  deriving (Val, Inherit, Auto, Other)

sansSerif, serif, monospace, cursive, fantasy :: GenericFontFamily

sansSerif = GenericFontFamily "sans-serif"
serif     = GenericFontFamily "serif"
monospace = GenericFontFamily "monospace"
cursive   = GenericFontFamily "cursive"
fantasy   = GenericFontFamily "fantasy"

-- | The `fontFamily` style rules takes to lists of font families: zero or more
-- custom font-families and preferably one or more generic font families.

fontFamily :: [Text] -> [GenericFontFamily] -> Css
fontFamily a b = key "font-family" $
  let sep = if null a || null b then "" else ", "
   in value (Literal <$> a) <> sep <> value b

-------------------------------------------------------------------------------

newtype FontSize = FontSize Value
  deriving (Val, Inherit, Auto, Other)

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
  deriving (Val, Inherit, Normal, Other)

italic, oblique :: FontStyle

italic = FontStyle "italic"
oblique = FontStyle "oblique"

fontStyle :: FontStyle -> Css
fontStyle = key "font-style"

-------------------------------------------------------------------------------

newtype FontVariant = FontVariant Value
  deriving (Val, Inherit, Normal, Other)

smallCaps :: FontVariant
smallCaps = FontVariant "small-caps"

fontVariant :: FontVariant -> Css
fontVariant = key "font-variant"

-------------------------------------------------------------------------------

newtype FontWeight = FontWeight Value
  deriving (Val, Inherit, Normal, Other)

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
  deriving (Val, Other)

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

