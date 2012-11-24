{-# LANGUAGE OverloadedStrings #-}
module Selector where

import Data.Text (Text)
import Data.Monoid
import Data.String
import Prelude hiding ((>), (+))

import qualified Data.Text as Text

data Selector
  = None
  | Star
  | Elem     Text
  | Id       Text
  | Class    Text
  | Child    Selector Selector
  | Deep     Selector Selector
  | Adjacent Selector Selector
  | Combined Selector Selector
  deriving (Eq, Ord, Show)

instance IsString Selector where
  fromString = fromText . fromString

fromText :: Text -> Selector
fromText t =
  case Text.uncons t of
    Just ('#', s) -> Id    s
    Just ('.', s) -> Class s
    _             -> Elem  t

instance Monoid Selector where
  mempty  = None
  mappend = Combined

(>>) :: Selector -> Selector -> Selector
(>>) = Deep

(>) :: Selector -> Selector -> Selector
(>) = Child

(+) :: Selector -> Selector -> Selector
(+) = Adjacent

