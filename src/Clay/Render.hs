{-# LANGUAGE OverloadedStrings #-}
module Clay.Render
( Config(..)
, pretty
, compact
, css
, cssWith
)
where

import Control.Applicative
import Control.Monad.Writer
import Data.Either
import Data.Foldable (foldMap)
import Data.List (sort)
import Data.Text (Text)
import Data.Text.Lazy.Builder
import Prelude hiding (filter)

import qualified Data.Text         as Text
import qualified Data.Text.Lazy.IO as Text

import Clay.Rule hiding (Child)
import Clay.Property
import Clay.Selector

import qualified Clay.Rule as Rule

data Config = Config
  { indentation :: Builder
  , newline     :: Builder
  , sep         :: Builder
  , warn        :: Bool
  , align       :: Bool
  }

pretty :: Config
pretty = Config "  " "\n" " " True True

compact :: Config
compact = Config "" "" "" False False

css :: Css -> IO ()
css = cssWith pretty []

cssWith :: Config -> [Rule] -> Css -> IO ()
cssWith cfg top
  = Text.putStr
  . toLazyText
  . rules cfg top
  . execWriter

rules :: Config -> [Rule] -> Rules -> Builder
rules cfg sel (Rules rs) = mconcat
  [ rule cfg sel (lefts rs)
  , newline cfg
  , foldMap (\(a, b) -> rules cfg (a : sel) b) (rights rs)
  ]

rule :: Config -> [Rule] -> [(Key (), Value)] -> Builder
rule _   _   []    = mempty
rule cfg sel props =
  let xs = collect =<< props
   in mconcat
      [ renderRule sel
      , newline cfg
      , "{"
      , newline cfg
      , properties cfg xs
      , "}"
      , newline cfg
      ]

renderRule :: [Rule] -> Builder
renderRule = selector . merger

merger :: [Rule] -> Selector
merger []     = error "this should be fixed!"
merger (x:xs) =
  case x of
    Rule.Child s -> case xs of [] -> s; _  -> merger xs |> s
    Sub        s  -> case xs of [] -> s; _  -> merger xs `deep` s
    Root       s  -> s `deep` merger xs
    Pop        i  -> merger (drop i (x:xs))
    Self       f  -> merger xs `with` f

collect :: (Key (), Value) -> [Either Text (Text, Text)]
collect (Key ky, Value vl) =
  case (ky, vl) of
    ( Plain    k  , Plain    v  ) -> [prop k v]
    ( Prefixed ks , Plain    v  ) -> flip map ks $ \(p, k) -> prop (p <> k) v
    ( Plain    k  , Prefixed vs ) -> flip map vs $ \(p, v) -> prop k (p <> v)
    ( Prefixed ks , Prefixed vs ) -> flip map ks $ \(p, k) -> (Left (p <> k) `maybe` prop (p <> k)) (lookup p vs)
  where prop k v = Right (k, v)

properties :: Config -> [Either Text (Text, Text)] -> Builder
properties cfg xs =
  let width = 1 + maximum (Text.length . fst <$> rights xs)
      ind   = indentation cfg
      new   = newline cfg
   in flip foldMap xs $ \p ->
        case p of
          Left w -> if warn cfg then ind <> "/* no value for " <> fromText w <> " */" <> new else mempty
          Right (k, v) ->
            let pad = if align cfg then fromText (Text.replicate (width - Text.length k) " ") else ""
             in mconcat [ind, fromText k, pad, ":", sep cfg, fromText v, ";", new]

selector :: Selector -> Builder
selector = intersperse ",\n" . rec
  where rec (In (Filtered ft p)) = (<> filter ft) <$>
          case p of
            Star           -> ["*"]
            Elem t         -> [fromText t]
            Child      a b -> ins " > " <$> rec a <*> rec b
            Deep       a b -> ins " "   <$> rec a <*> rec b
            Adjacent   a b -> ins " + " <$> rec a <*> rec b
            Combined   a b -> join ((:) <$> rec a <*> (pure <$> rec b))
          where ins s a b = (a <> s <> b)

predicate :: Predicate -> Builder
predicate ft = mconcat $
  case ft of
    Id         a   -> [ "#", fromText a                                             ]
    Class      a   -> [ ".", fromText a                                             ]
    Attr       a   -> [ "[", fromText a,                     "]"                    ]
    AttrVal    a v -> [ "[", fromText a,  "='", fromText v, "']"                    ]
    AttrEnds   a v -> [ "[", fromText a, "$='", fromText v, "']"                    ]
    AttrSpace  a v -> [ "[", fromText a, "~='", fromText v, "']"                    ]
    AttrHyph   a v -> [ "[", fromText a, "|='", fromText v, "']"                    ]
    Pseudo     a   -> [ ":", fromText a                                             ]
    PseudoFunc a p -> [ ":", fromText a, "(", intersperse "," (map fromText p), ")" ]

filter :: Filter -> Builder
filter = foldMap predicate . sort . unFilter

