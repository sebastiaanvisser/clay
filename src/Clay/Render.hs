{-# LANGUAGE OverloadedStrings #-}
module Clay.Render
( Config (..)
, pretty
, compact
, render
, putCss
, renderWith
)
where

import Control.Applicative
import Control.Monad.Writer
import Data.Either
import Data.Foldable (foldMap)
import Data.List (sort)
import Data.Maybe
import Data.Text (Text)
import Data.Text.Lazy.Builder
import Prelude hiding ((**))

import qualified Data.Text         as Text
import qualified Data.Text.Lazy    as Lazy
import qualified Data.Text.Lazy.IO as Lazy

import Clay.Stylesheet hiding (Child, query)
import Clay.Property
import Clay.Selector

import qualified Clay.Stylesheet as Rule

data Config = Config
  { indentation :: Builder
  , newline     :: Builder
  , sep         :: Builder
  , warn        :: Bool
  , align       :: Bool
  , banner      :: Bool
  }

-- | Configuration to print to a pretty human readable CSS output.

pretty :: Config
pretty = Config "  " "\n" " " True True True

-- | Configuration to print to a compacted unreadable CSS output.

compact :: Config
compact = Config "" "" "" False False False

-- | Render to CSS using the default configuration (`pretty`) and directly
-- print to the standard output.

putCss :: Css -> IO ()
putCss = Lazy.putStr . render

-- | Render a stylesheet with the default configuration. The pretty printer is
-- used by default.

render :: Css -> Lazy.Text
render = renderWith pretty []

-- | Render a stylesheet with a custom configuration and an optional outer
-- scope.

renderWith :: Config -> [App] -> Css -> Lazy.Text
renderWith cfg top (S c)
  = renderBanner cfg
  . toLazyText
  . rules cfg top
  . execWriter
  $ c

renderBanner :: Config -> Lazy.Text -> Lazy.Text
renderBanner cfg =
  if banner cfg
  then (<> "\n/* Generated with Clay, http://fvisser.nl/clay */")
  else id

query :: Config -> MediaQuery -> [App] -> [Rule] -> Builder
query cfg q sel rs =
  mconcat
    [ mediaQuery q
    , newline cfg
    , "{"
    , newline cfg
    , rules cfg sel rs
    , "}"
    , newline cfg
    ]

mediaQuery :: MediaQuery -> Builder
mediaQuery (MediaQuery no ty fs) = mconcat
  [ "@media "
  , case no of
      Nothing   -> ""
      Just Not  -> "not "
      Just Only -> "only "
  , mediaType ty
  , mconcat ((" and " <>) . feature <$> fs)
  ]

mediaType :: MediaType -> Builder
mediaType (MediaType (Value v)) = fromText (plain v)

feature :: Feature -> Builder
feature (Feature k mv) =
  case mv of
    Nothing        -> fromText k
    Just (Value v) -> mconcat
      [ "(" , fromText k , ": " , fromText (plain v) , ")" ]

face :: Config -> [Rule] -> Builder
face cfg rs = mconcat
  [ "@font-face"
  , rules cfg [] rs
  ]

rules :: Config -> [App] -> [Rule] -> Builder
rules cfg sel rs = mconcat
  [ rule cfg sel (mapMaybe property rs)
  , newline cfg
  , (\(a, b) -> rules cfg (a : sel) b) `foldMap` mapMaybe nested  rs
  , (\(a, b) -> query cfg  a   sel  b) `foldMap` mapMaybe queries rs
  , (\ns     -> face  cfg          ns) `foldMap` mapMaybe faces   rs
  ]
  where property (Property k v) = Just (k, v)
        property _              = Nothing
        nested   (Nested a ns ) = Just (a, ns)
        nested   _              = Nothing
        queries  (Query q ns  ) = Just (q, ns)
        queries  _              = Nothing
        faces    (Face ns     ) = Just ns
        faces    _              = Nothing

rule :: Config -> [App] -> [(Key (), Value)] -> Builder
rule _   _   []    = mempty
rule cfg sel props =
  let xs = collect =<< props
   in mconcat
      [ selector cfg (merger sel)
      , newline cfg
      , "{"
      , newline cfg
      , properties cfg xs
      , "}"
      , newline cfg
      ]

merger :: [App] -> Selector
merger []     = "" -- error "this should be fixed!"
merger (x:xs) =
  case x of
    Rule.Child s -> case xs of [] -> s; _  -> merger xs |> s
    Sub        s -> case xs of [] -> s; _  -> merger xs ** s
    Root       s -> s ** merger xs
    Pop        i -> merger (drop i (x:xs))
    Self       f -> case xs of [] -> star `with` f; _ -> merger xs `with` f

collect :: (Key (), Value) -> [Either Text (Text, Text)]
collect (Key ky, Value vl) =
  case (ky, vl) of
    ( Plain    k  , Plain    v  ) -> [prop k v]
    ( Prefixed ks , Plain    v  ) -> flip map ks $ \(p, k) -> prop (p <> k) v
    ( Plain    k  , Prefixed vs ) -> flip map vs $ \(p, v) -> prop k (p <> v)
    ( Prefixed ks , Prefixed vs ) -> flip map ks $ \(p, k) -> (Left (p <> k) `maybe` (prop (p <> k) . mappend p)) (lookup p vs)
  where prop k v = Right (k, v)

properties :: Config -> [Either Text (Text, Text)] -> Builder
properties cfg xs =
  let width = 1 + maximum (Text.length . fst <$> rights xs)
      ind   = indentation cfg
      new   = newline cfg
   in (<> new) $ intersperse (";" <> new) $ flip map xs $ \p ->
        case p of
          Left w -> if warn cfg then ind <> "/* no value for " <> fromText w <> " */" <> new else mempty
          Right (k, v) ->
            let pad = if align cfg then fromText (Text.replicate (width - Text.length k) " ") else ""
             in mconcat [ind, fromText k, pad, ":", sep cfg, fromText v]

selector :: Config -> Selector -> Builder
selector cfg = intersperse ("," <> newline cfg) . rec
  where rec (In (SelectorF (Refinement ft) p)) = (<> foldMap predicate (sort ft)) <$>
          case p of
            Star           -> if length ft == 0 then ["*"] else [""]
            Elem t         -> [fromText t]
            Child      a b -> ins " > " <$> rec a <*> rec b
            Deep       a b -> ins " "   <$> rec a <*> rec b
            Adjacent   a b -> ins " + " <$> rec a <*> rec b
            Combined   a b -> rec a ++ rec b
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

