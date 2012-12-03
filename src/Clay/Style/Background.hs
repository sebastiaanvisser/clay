{-# LANGUAGE OverloadedStrings, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Clay.Style.Background where

import Data.Text (Text)
import Data.Monoid
import Data.Maybe
import Prelude hiding (Left, Right, repeat)

import Clay.Core.Property
import Clay.Core.Rule
import Clay.Style.Color
import Clay.Style.Common
import Clay.Style.Size

backgroundColor :: Color -> Css
backgroundColor = key "background-color"

-------------------------------------------------------------------------------

newtype Side = Side Value
  deriving Val

instance Inherit Side where inherit = Side "inherit"
instance Other   Side where other   = Side

posTop, posLeft, posRight, posBottom, posCenter, posMiddle :: Side

posTop    = Side "top"
posLeft   = Side "left"
posRight  = Side "right"
posBottom = Side "bottom"
posCenter = Side "center"
posMiddle = Side "middle"

newtype BackgroundPosition = BackgroundPosition Value
  deriving Val

placed :: Side -> Side -> BackgroundPosition
placed a b = BackgroundPosition (value (a, b))

positioned :: Size a -> Size a -> BackgroundPosition
positioned a b = BackgroundPosition (value (a, b))

instance Inherit BackgroundPosition where inherit = BackgroundPosition "inherit"
instance Other   BackgroundPosition where other   = BackgroundPosition

backgroundPosition :: BackgroundPosition -> Css
backgroundPosition = key "background-position"

-------------------------------------------------------------------------------

newtype BackgroundSize = BackgroundSize Value
  deriving Val

contain, cover :: BackgroundSize

contain = BackgroundSize "contain"
cover   = BackgroundSize "cover"

sized :: Size a -> Size a -> BackgroundSize
sized a b = BackgroundSize (value (a, b))

instance Inherit BackgroundSize where inherit = BackgroundSize "inherit"
instance Auto    BackgroundSize where auto    = sized auto auto
instance Other   BackgroundSize where other   = BackgroundSize

backgroundSize :: BackgroundSize -> Css
backgroundSize = key "background-size"

-------------------------------------------------------------------------------

newtype BackgroundRepeat = BackgroundRepeat Value
  deriving Val

instance Other BackgroundRepeat where other = BackgroundRepeat

repeat, space, round, noRepeat :: BackgroundRepeat

repeat   = BackgroundRepeat "repeat"
space    = BackgroundRepeat "space"
round    = BackgroundRepeat "round"
noRepeat = BackgroundRepeat "no-repeat"

xyRepeat :: BackgroundRepeat -> BackgroundRepeat -> BackgroundRepeat
xyRepeat a b = BackgroundRepeat (value (a, b))

repeatX :: BackgroundRepeat
repeatX = xyRepeat repeat noRepeat

repeatY :: BackgroundRepeat
repeatY = xyRepeat noRepeat repeat

backgroundRepeat :: BackgroundRepeat -> Css
backgroundRepeat = key "background-repeat"

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

-- backgroundOrigin     = key "background-origin"
-- backgroundClip       = key "background-clip"
-- backgroundAttachment = key "background-attachment"

-------------------------------------------------------------------------------
-- Background property as a type class.

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

