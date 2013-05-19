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

, GenericFontFamily
, fontFamily
, serif
, sansSerif
, cursive
, fantasy
, monospace

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
import Data.Text (pack)
import Data.Monoid
import Prelude hiding (Left, Right)
import Data.Text (Text)
import Data.String (IsString (..))

import Clay.Color
import Clay.Common
import Clay.Property
import Clay.Stylesheet
import Clay.Size

-------------------------------------------------------------------------------

-- | The five generic font families.
--
-- <http://www.w3.org/TR/css3-fonts/#generic-font-families>.

data GenericFontFamily = Serif | SansSerif | Cursive | Fantasy | Monospace

instance Val GenericFontFamily where
  value g = Value $ Plain $ case g of
    Serif     -> "serif"
    SansSerif -> "sans-serif"
    Cursive   -> "cursive"
    Fantasy   -> "fantasy"
    Monospace -> "monospace"

instance IsString GenericFontFamily where
  fromString s = case s of
    "serif"      -> Serif
    "sans-serif" -> SansSerif
    "cursive"    -> Cursive
    "fantasy"    -> Fantasy
    "monospace"  -> Monospace
    _            -> error $ "unknown generic font family \"" ++ s ++ "\""

-------------------------------------------------------------------------------

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

data Required a = Required
  { requiredFontSize          :: Size a
  , requiredLineHeight        :: (Maybe (Size a))
  , requiredFontFamily        :: [Text]
  , requiredGenericFontFamily :: [GenericFontFamily]
  }

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

fontFamily :: [Text] -> [GenericFontFamily] -> Css
fontFamily familyNames genericFamilies = key "font-family" $
  let sep = if null familyNames || null genericFamilies then "" else ","
   in value (Literal <$> familyNames) <> sep <> value genericFamilies

serif, sansSerif, cursive, fantasy, monospace :: GenericFontFamily

serif     = Serif
sansSerif = SansSerif
cursive   = Cursive
fantasy   = Fantasy
monospace = Monospace

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

