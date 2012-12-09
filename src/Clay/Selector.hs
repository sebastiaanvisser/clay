{-# LANGUAGE
    OverloadedStrings
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  #-}
module Clay.Selector where

import Control.Applicative
import Data.Monoid
import Data.String
import Data.Text (Text)
import Prelude hiding (foldl)

import qualified Data.Text as Text

-- The generic selector DSL.

id_ :: Text -> Filter
id_ = Filter . pure . Id

class_ :: Text -> Filter
class_ = Filter . pure . Class

pseudo :: Text -> Filter
pseudo = Filter . pure . Pseudo

func :: Text -> [Text] -> Filter
func f = Filter . pure . PseudoFunc f

attr :: Text -> Filter
attr = Filter . pure . Attr

(@=) :: Text -> Text -> Filter
(@=) a = Filter . pure . AttrVal a

($=) :: Text -> Text -> Filter
($=) a = Filter . pure . AttrEnds a

(~=) :: Text -> Text -> Filter
(~=) a = Filter . pure . AttrSpace a

(|=) :: Text -> Text -> Filter
(|=) a = Filter . pure . AttrHyph a

star :: Selector
star = In (Filtered mempty Star)

with :: Selector -> Filter -> Selector
with (In (Filtered fs e)) ps = In (Filtered (fs <> ps) e)

(|>) :: Selector -> Selector -> Selector
(|>) a b = In (Filtered mempty (Child a b))

(|+) :: Selector -> Selector -> Selector
(|+) a b = In (Filtered mempty (Adjacent a b))

deep :: Selector -> Selector -> Selector
deep a b = In (Filtered mempty (Deep a b))

-------------------------------------------------------------------------------

data Predicate
  = Id         Text
  | Class      Text
  | Attr       Text
  | AttrVal    Text Text
  | AttrEnds   Text Text
  | AttrSpace  Text Text
  | AttrHyph   Text Text
  | Pseudo     Text
  | PseudoFunc Text [Text]
  deriving (Eq, Ord, Show)

newtype Filter = Filter { unFilter :: [Predicate] }
  deriving (Monoid, Eq, Ord, Show)

instance IsString Filter where
  fromString = predicateFromText . fromString

predicateFromText :: Text -> Filter
predicateFromText t = Filter $
  case Text.uncons t of
    Just ('#', s) -> [Id     s]
    Just ('.', s) -> [Class  s]
    Just (':', s) -> [Pseudo s]
    Just ('@', s) -> [Attr   s]
    _             -> [Attr   t]

-------------------------------------------------------------------------------

data Path f
  = Star
  | Elem      Text
  | Child     f f
  | Deep      f f
  | Adjacent  f f
  | Combined  f f

newtype Fix f = In { out :: f (Fix f) }

data Filtered a = Filtered Filter (Path a)

type Selector = Fix Filtered

instance IsString (Fix Filtered) where
  fromString = text . fromString

text :: Text -> Selector
text t = In $
  case Text.uncons t of
    Just ('#', s) -> Filtered (Filter [Id s]) Star
    Just ('.', s) -> Filtered (Filter [Class s]) Star
    _             -> Filtered mempty (Elem t)

instance Monoid (Fix Filtered) where
  mempty      = error "Selector is a semigroup"
  mappend a b = In (Filtered mempty (Combined a b))

