{-# LANGUAGE OverloadedStrings #-}
module Clay.Render (css, cssIn) where

import Control.Monad.Writer
import Data.Either
import Data.Foldable
import Data.Text.Lazy.Builder

import qualified Data.Text.Lazy.IO as Text

import Clay.Rule
import Clay.Property
import Clay.Selector hiding (Child)

import qualified Clay.Selector as Selector

css :: Css -> IO ()
css = cssIn []

cssIn :: [Rule] -> Css -> IO ()
cssIn top = Text.putStr . toLazyText . rules top . execWriter

rules :: [Rule] -> Rules -> Builder
rules sel (Rules rs) = mconcat
  [ rule sel (lefts rs)
  , "\n"
  , foldMap (\(a, b) -> rules (a : sel) b) (rights rs)
  ]

rule :: [Rule] -> [(Key (), Value)] -> Builder
rule _   []    = mempty
rule sel props = mconcat
  [ renderRule sel
  , "\n{\n"
  , foldMap property props
  , "}\n"
  ]

renderRule :: [Rule] -> Builder
renderRule = Selector.render . merger

merger :: [Rule] -> Selector
merger []     = error "this should be fixed!"
merger (x:xs) =
  case x of
    Child s -> case xs of
                 [] -> s
                 _  -> merger xs |> s
    Sub  s  -> case xs of
                 [] -> s
                 _  -> merger xs `deep` s
    Root s  -> s `deep` merger xs
    Pop  i  -> merger (drop i (x:xs))
    Self f  -> merger xs `with` f

property :: (Key (), Value) -> Builder
property (Key ky, Value vl) =
  case (ky, vl) of
    ( Plain    k  , Plain    v  ) -> prop k v
    ( Prefixed ks , Plain    v  ) -> for ks $ \(p, k) -> prop (p <> k) v
    ( Plain    k  , Prefixed vs ) -> for vs $ \(p, v) -> prop k (p <> v)
    ( Prefixed ks , Prefixed vs ) -> for ks $ \(p, k) -> (noImpl p k `maybe` prop (p <> k)) (lookup p vs)
  where prop k v = mconcat ["  ", fromText k, ": ", fromText v, ";\n"]
        for = flip foldMap
        noImpl p k = "  /* no value for " <> fromText p <> fromText k <> " */\n"

