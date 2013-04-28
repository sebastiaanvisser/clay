{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  #-}

-- | A bunch of type classes representing common values shared between multiple
-- CSS properties, like `Auto`, `Inherit`, `None`, `Normal` and several more.
--
-- All the common value type classes have an instance for the Value type,
-- making them easily derivable for custom value types.


module Clay.Common where

import Clay.Property
import Data.Text (Text)

-------------------------------------------------------------------------------

class All     a where all     ::          a
class Auto    a where auto    ::          a
class Inherit a where inherit ::          a
class None    a where none    ::          a
class Normal  a where normal  ::          a
class Visible a where visible ::          a
class Hidden  a where hidden  ::          a

-- | The other type class is used to escape from the type safety introduced by
-- embedding CSS properties into the typed world of Clay. `Other` allows you to
-- cast any `Value` to a specific value type.

class Other   a where other   :: Value -> a

instance All     Value where all     = "all"
instance Auto    Value where auto    = "auto"
instance Inherit Value where inherit = "inherit"
instance Normal  Value where normal  = "normal"
instance None    Value where none    = "none"
instance Visible Value where visible = "visible"
instance Hidden  Value where hidden  = "hidden"
instance Other   Value where other   = id

-------------------------------------------------------------------------------

-- | Common list browser prefixes to make experimental properties work in
-- different browsers.

browsers :: Prefixed
browsers = Prefixed
  [ ( "-webkit-", "" )
  , (    "-moz-", "" )
  , (     "-ms-", "" )
  , (      "-o-", "" )
  , (         "", "" )
  ]

-------------------------------------------------------------------------------

newtype FontStyle = FontStyle Value
  deriving (Show, Val, Inherit, Normal)

newtype FontVariant = FontVariant Value
  deriving (Show, Val, Inherit, Normal)

newtype FontWeight = FontWeight Value
  deriving (Show, Val, Inherit, Normal)


data FontFaceFormat = WOFF | TrueType | OpenType | EmbeddedOpenType | SVG
  deriving Show

data FontFaceSrc
  = FontFaceSrcUrl Text (Maybe FontFaceFormat)
  | FontFaceSrcLocal Text
  deriving Show

data FontFace = FontFace
  { fontFaceFamily  :: Text
  , fontFaceSrc     :: [FontFaceSrc]
  , fontFaceVariant :: Maybe FontVariant
  , fontFaceWeight  :: Maybe FontWeight
  , fontFaceStyle   :: Maybe FontStyle
  } deriving Show
