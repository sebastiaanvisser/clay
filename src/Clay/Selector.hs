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

-- | The star selector applies to all elements. Maps to @*@ in CSS.

star :: Selector
star = In (SelectorF (Refinement []) Star)

-- | Select elements by name. The preferred syntax is to enable
-- @OverloadedStrings@ and actually just use @\"element-name\"@ or use one of
-- the predefined elements from "Clay.Elements".

element :: Text -> Selector
element e = In (SelectorF (Refinement []) (Elem e))

-- | The deep selector composer. Maps to @sel1 sel2@ in CSS.

(**) :: Selector -> Selector -> Selector
(**) a b = In (SelectorF (Refinement []) (Deep a b))

-- | The child selector composer. Maps to @sel1 > sel2@ in CSS.

(|>) :: Selector -> Selector -> Selector
(|>) a b = In (SelectorF (Refinement []) (Child a b))

-- | The adjacent selector composer. Maps to @sel1 + sel2@ in CSS.

(|+) :: Selector -> Selector -> Selector
(|+) a b = In (SelectorF (Refinement []) (Adjacent a b))

-- | The filter selector composer, adds a filter to a selector. Maps to
-- something like @sel#filter@ or @sel.filter@ in CSS, depending on the filter.

with :: Selector -> Refinement -> Selector
with (In (SelectorF (Refinement fs) e)) (Refinement ps) = In (SelectorF (Refinement (fs ++ ps)) e)

-- | Filter elements by id. The preferred syntax is to enable
-- @OverloadedStrings@ and use @\"#id-name\"@.

byId :: Text -> Refinement
byId = Refinement . pure . Id

-- | Filter elements by class. The preferred syntax is to enable
-- @OverloadedStrings@ and use @\".class-name\"@.

byClass :: Text -> Refinement
byClass = Refinement . pure . Class

-- | Filter elements by pseudo selector or pseudo class. The preferred syntax
-- is to enable @OverloadedStrings@ and use @\":pseudo-selector\"@ or use one
-- of the predefined ones from "Clay.Pseudo".

pseudo :: Text -> Refinement
pseudo = Refinement . pure . Pseudo

-- | Filter elements by pseudo selector functions. The preferred way is to use
-- one of the predefined functions from "Clay.Pseudo".

func :: Text -> [Text] -> Refinement
func f = Refinement . pure . PseudoFunc f

attr :: Text -> Refinement
attr = Refinement . pure . Attr

(@=) :: Text -> Text -> Refinement
(@=) a = Refinement . pure . AttrVal a

($=) :: Text -> Text -> Refinement
($=) a = Refinement . pure . AttrEnds a

(~=) :: Text -> Text -> Refinement
(~=) a = Refinement . pure . AttrSpace a

(|=) :: Text -> Text -> Refinement
(|=) a = Refinement . pure . AttrHyph a

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

newtype Refinement = Refinement { unFilter :: [Predicate] }

instance IsString Refinement where
  fromString = filterFromText . fromString

filterFromText :: Text -> Refinement
filterFromText t = Refinement $
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

data SelectorF a = SelectorF Refinement (Path a)

type Selector = Fix SelectorF

instance IsString (Fix SelectorF) where
  fromString = text . fromString

text :: Text -> Selector
text t = In $
  case Text.uncons t of
    Just ('#', s) -> SelectorF (Refinement [Id s]) Star
    Just ('.', s) -> SelectorF (Refinement [Class s]) Star
    _             -> SelectorF (Refinement []) (Elem t)

instance Monoid (Fix SelectorF) where
  mempty      = error "Selector is a semigroup"
  mappend a b = In (SelectorF (Refinement []) (Combined a b))

