{-# LANGUAGE
    OverloadedStrings
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  #-}
module Clay.Core.Selector where

import Data.String
import Prelude hiding (foldl)
import Control.Applicative
import Control.Monad
import Data.Foldable hiding (elem)
import Data.List (sort)
import Data.Monoid
import Data.Text (Text)
import Data.Text.Lazy.Builder hiding (fromString)

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

-- List of specific pseudo classes, from:
-- https://developer.mozilla.org/en-US/docs/CSS/Pseudo-classes

link, visited, active, hover, focus, firstChild :: Filter

link       = ":link"
visited    = ":visited"
active     = ":active"
hover      = ":hover"
focus      = ":focus"
firstChild = ":first-child"

firstOfType, lastOfType, empty, target, checked, enabled, disabled :: Filter

firstOfType = ":first-of-type"
lastOfType  = ":last-of-type"
empty       = ":empty"
target      = ":target"
checked     = ":checked"
enabled     = ":enabled"
disabled    = ":disabled"

nthChild, nthLastChild, nthOfType :: Text -> Filter

nthChild     n = func "nth-child"      [n]
nthLastChild n = func "nth-last-child" [n]
nthOfType    n = func "nth-of-type"    [n]

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

renderPredicate :: Predicate -> Builder
renderPredicate ft = mconcat $
  case ft of
    Id         a   -> [ "#", fromText a                                              ]
    Class      a   -> [ ".", fromText a                                              ]
    Attr       a   -> [ "[", fromText a,                     "]"                     ]
    AttrVal    a v -> [ "[", fromText a,  "='", fromText v, "']"                     ]
    AttrEnds   a v -> [ "[", fromText a, "$='", fromText v, "']"                     ]
    AttrSpace  a v -> [ "[", fromText a, "~='", fromText v, "']"                     ]
    AttrHyph   a v -> [ "[", fromText a, "|='", fromText v, "']"                     ]
    Pseudo     a   -> [ ":", fromText a                                              ]
    PseudoFunc a p -> [ ":", fromText a, "(", intersperse "," (map fromText p) , ")" ]

newtype Filter = Filter { unFilter :: [Predicate] }
  deriving (Monoid, Eq, Ord, Show)

renderFilter :: Filter -> Builder
renderFilter = foldMap renderPredicate . sort . unFilter

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
  | Elem       Text
  | Child      f f
  | Deep       f f
  | Adjacent   f f
  | Combined   f f

newtype Fix f = In { out :: f (Fix f) }

data Filtered a = Filtered Filter (Path a)

type Selector = Fix Filtered

render :: Selector -> Builder
render = intersperse ",\n" . renderSelector

renderSelector :: Selector -> [Builder]
renderSelector (In (Filtered ft p)) = (<> renderFilter ft) <$>
  case p of
    Star           -> ["*"]
    Elem t         -> [fromText t]
    Child      a b -> ins " > " <$> renderSelector a <*> renderSelector b
    Deep       a b -> ins " "   <$> renderSelector a <*> renderSelector b
    Adjacent   a b -> ins " + " <$> renderSelector a <*> renderSelector b
    Combined   a b -> join ((:) <$> renderSelector a <*> (pure <$> renderSelector b))
  where ins s a b = (a <> s <> b)

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

-------------------------------------------------------------------------------
-- Helper functions.

intersperse :: Monoid a => a -> [a] -> a
intersperse _ []     = mempty
intersperse s (x:xs) = foldl (\a b -> a <> s <> b) x xs

