{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Clay.Style.Background where

import Data.Text (Text)
import Data.Monoid
import Data.Maybe
import Prelude hiding (Left, Right)

import Clay.Style.Color
import Clay.Style.Size
import Clay.Core.Property
import Clay.Core.Rule

data Side = Top | Left | Right | Bottom | Center

instance Val Side where
  value Top    = "top"
  value Left   = "left"
  value Right  = "right"
  value Bottom = "bottom"
  value Center = "center"

data BackgroundPosition
  = Place Side Side
  | Size  Size Size
  | Inherit

instance Val BackgroundPosition where
  value (Place a b) = value (a, b)
  value (Size  a b) = value (a, b)
  value Inherit     = "inherit"

data BackgroundSize
  = Contain
  | Cover
  | Length (Maybe Size) (Maybe Size)

instance Val BackgroundSize where
  value Contain      = "contain"
  value Cover        = "cover"
  value (Length a b) = value (maybe "auto" value a, maybe "auto" value b)

data BackgroundRepeat
  = Repeat
  | Space
  | Round
  | NoRepeat

instance Val BackgroundRepeat where
  value Repeat   = "repeat"
  value Space    = "space"
  value Round    = "round"
  value NoRepeat = "no-repeat"

repeat :: (BackgroundRepeat, BackgroundRepeat)
repeat = (Repeat, Repeat)

repeatX :: (BackgroundRepeat, BackgroundRepeat)
repeatX = (Repeat, NoRepeat)

repeatY :: (BackgroundRepeat, BackgroundRepeat)
repeatY = (NoRepeat, Repeat)

space :: (BackgroundRepeat, BackgroundRepeat)
space = (Space, Space)

round :: (BackgroundRepeat, BackgroundRepeat)
round = (Round, Round)

noRepeat :: (BackgroundRepeat, BackgroundRepeat)
noRepeat = (NoRepeat, NoRepeat)

backgroundColor :: Color -> Css
backgroundColor = key "background-color"

backgroundPosition :: BackgroundPosition -> Css
backgroundPosition = key "background-position"

backgroundSize :: BackgroundSize -> Css
backgroundSize = key "background-size"

backgroundRepeat :: (BackgroundRepeat, BackgroundRepeat) -> Css
backgroundRepeat = key "background-repeat"

data Gradient = Grad

data BackgroundImage
  = Url Text
  -- | Gradient Gradient

url :: Text -> Maybe BackgroundImage
url = Just . Url

instance Val BackgroundImage where
  value (Url u) = Value ("url(\"" <> u <> "\")")

backgroundImage :: Maybe BackgroundImage -> Css
backgroundImage = key "background-image"

-- backgroundOrigin     = key "background-origin"
-- backgroundClip       = key "background-clip"
-- backgroundAttachment = key "background-attachment"

class Val a => Background a where
  background :: a -> Css
  background = key "background"

instance Background Color
instance Background BackgroundPosition
instance Background BackgroundImage
instance Background BackgroundSize
instance Background (BackgroundPosition, BackgroundPosition)
instance Background BackgroundRepeat
instance Background (BackgroundRepeat, BackgroundRepeat)

