{-# LANGUAGE OverloadedStrings #-}
module Clay.Core.Render (css, cssIn) where

import Control.Monad.Writer
import Data.Either
import Data.Foldable
import Data.Text (Text)
import Data.Text.Lazy.Builder

import qualified Data.Text.Lazy.IO as Text

import Clay.Core.Rule     (Css, Rule(..), Rules (Rules))
import Clay.Core.Selector hiding (Child)

import qualified Clay.Core.Selector as Selector

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

rule :: [Rule] -> [(Text, Text)] -> Builder
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

property :: (Text, Text) -> Builder
property (key, val) =
  mconcat ["  ", fromText key, ": ", fromText val, ";\n"]

