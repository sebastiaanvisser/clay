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
import Data.Text (Text)
import Data.Text.Lazy.Builder

import qualified Data.Text         as Text
import qualified Data.Text.Lazy.IO as Text

import Clay.Rule
import Clay.Property
import Clay.Selector hiding (Child)

import qualified Clay.Selector as Selector

data Config = Config
  { indent  :: Builder
  , newline :: Builder
  , sep     :: Builder
  , warn    :: Bool
  , align   :: Bool
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
renderRule = Selector.render . merger

merger :: [Rule] -> Selector
merger []     = error "this should be fixed!"
merger (x:xs) =
  case x of
    Child s -> case xs of [] -> s; _  -> merger xs |> s
    Sub  s  -> case xs of [] -> s; _  -> merger xs `deep` s
    Root s  -> s `deep` merger xs
    Pop  i  -> merger (drop i (x:xs))
    Self f  -> merger xs `with` f

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
      ind   = indent cfg
      new   = newline cfg
   in flip foldMap xs $ \p ->
        case p of
          Left w -> if warn cfg then ind <> "/* no value for " <> fromText w <> " */" <> new else mempty
          Right (k, v) ->
            let pad = if align cfg then fromText (Text.replicate (width - Text.length k) " ") else ""
             in mconcat [ind, fromText k, pad, ":", sep cfg, fromText v, ";", new]


