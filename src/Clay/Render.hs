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
  { indentation    :: Builder
  , newline        :: Builder
  , sep            :: Builder
  , finalSemicolon :: Bool
  , warn           :: Bool
  , align          :: Bool
  , banner         :: Bool
  }

-- | Configuration to print to a pretty human readable CSS output.

pretty :: Config
pretty = Config
  { indentation    = "  "
  , newline        = "\n"
  , sep            = " "
  , finalSemicolon = True
  , warn           = True
  , align          = True
  , banner         = True
  }

-- | Configuration to print to a compacted unreadable CSS output.

compact :: Config
compact = Config
  { indentation    = ""
  , newline        = ""
  , sep            = ""
  , finalSemicolon = False
  , warn           = False
  , align          = False
  , banner         = False
  }

-- | Render to CSS using the default configuration (`pretty`) and directly
-- print to the standard output.

putCss :: Css -> IO ()
putCss = Lazy.putStr . render

-- | Render a stylesheet with the default configuration. The pretty printer is
-- used by default.

render :: Css -> Lazy.Text
render = renderWith pretty

-- | Render a stylesheet with a custom configuration and an optional outer
-- scope.

renderWith :: Config -> Css -> Lazy.Text
renderWith cfg (S c)
  = renderBanner cfg
  . toLazyText
  . cssRules cfg
  . flattenRules
  . execWriter
  $ c

-------------------------------------------------------------------------------

-- | The AST of a CSS3 file.

data Css3
  = Css3Query MediaQuery [Css3]
  | Css3Rule [App] [(Key (), Value)]
  | Css3Font       [(Key (), Value)]

flattenRules :: [Rule] -> [Css3]
flattenRules rules =
  let
    property p = case p of
      Property k v -> (k, v)
      _            -> error "only properties are allowed in @font-face"

    nestApp app css = case css of
      Css3Query q cs -> Css3Query q $ map (nestApp app) cs
      Css3Rule as ps -> Css3Rule (app:as) ps
      Css3Font ps    -> Css3Font ps

    (props, nests, qrys, faces) = foldr
      (\r (ps,ns,qs,fs) -> case r of
        Property k v -> ((k, v):ps,          ns,          qs,     fs)
        Nested a rs' -> (       ps, (a, rs'):ns,          qs,     fs)
        Query q  rs' -> (       ps,          ns, (q, rs'):qs,     fs)
        Face     rs' -> (       ps,          ns,          qs, rs':fs))
      ([],[],[],[])
      rules
  in
    (if null props then [] else [Css3Rule [] props])                      ++
    concatMap (\(app, rs') -> map (nestApp app) (flattenRules rs')) nests ++
    map       (\(q,   rs') -> Css3Query q (flattenRules rs'))       qrys  ++
    map       (\      rs'  -> Css3Font $ map property rs')          faces

-------------------------------------------------------------------------------

renderBanner :: Config -> Lazy.Text -> Lazy.Text
renderBanner cfg =
  if banner cfg
  then (<> "\n/* Generated with Clay, http://fvisser.nl/clay */")
  else id

-------------------------------------------------------------------------------

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

cssRules :: Config -> [Css3] -> Builder
cssRules cfg = (mconcat .) $ map $ \c -> mconcat $ case c of
  Css3Query  q cs -> [ mediaQuery q,                        block $ cssRules cfg cs ]
  Css3Rule sel ps -> [ selector cfg (merger $ reverse sel), propertyBlock ps        ]
  Css3Font     ps -> [ "@font-face",                        propertyBlock ps        ]
  where
    propertyBlock = block . properties cfg . concatMap collect
    block inner = mconcat [ nl, "{", nl, inner, "}", nl, nl ]
    nl = newline cfg

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
  let width     = 1 + maximum (Text.length . fst <$> rights xs)
      ind       = indentation cfg
      new       = newline cfg
      finalSemi = if finalSemicolon cfg then ";" else ""
   in (<> new) $ (<> finalSemi) $ intersperse (";" <> new) $ flip map xs $ \p ->
        case p of
          Left w -> if warn cfg then ind <> "/* no value for " <> fromText w <> " */" <> new else mempty
          Right (k, v) ->
            let pad = if align cfg then fromText (Text.replicate (width - Text.length k) " ") else ""
             in mconcat [ind, fromText k, pad, ":", sep cfg, fromText v]

selector :: Config -> Selector -> Builder
selector cfg = intersperse ("," <> newline cfg) . rec
  where rec (In (SelectorF (Refinement ft) p)) = (<> foldMap predicate (sort ft)) <$>
          case p of
            Star           -> if null ft then ["*"] else [""]
            Elem t         -> [fromText t]
            Child      a b -> ins " > " <$> rec a <*> rec b
            Deep       a b -> ins " "   <$> rec a <*> rec b
            Adjacent   a b -> ins " + " <$> rec a <*> rec b
            Combined   a b -> rec a ++ rec b
          where ins s a b = a <> s <> b

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

