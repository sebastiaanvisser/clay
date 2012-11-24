{-# LANGUAGE OverloadedStrings #-}
module Render where

import Control.Monad.Writer
import Data.Either
import Data.Foldable
import Data.Text (Text)
import Data.Text.Lazy.Builder

import qualified Data.Text.Lazy.IO as Text

import Rule     (Css, Rules (Rules))
import Selector (Selector (..))

css :: Css -> IO ()
css = cssIn None

cssIn :: Selector -> Css -> IO ()
cssIn top = Text.putStr . toLazyText . rules top . execWriter

rules :: Selector -> Rules -> Builder
rules sel (Rules rs) = mconcat
  [ rule sel (lefts rs)
  , "\n"
  , foldMap (\(a, b) -> rules (Deep sel a) b) (rights rs)
  ]

rule :: Selector -> [(Text, Text)] -> Builder
rule _   []    = mempty
rule sel props = mconcat
  [ selector sel
  , "\n{\n"
  , foldMap property props
  , "}\n"
  ]

selector :: Selector -> Builder
selector sel =
  case sel of
    None            -> mempty
    Star            -> singleton '*'
    Elem     t      -> fromText t
    Id       t      -> singleton '#' <> fromText t
    Class    t      -> singleton '.' <> fromText t
    Child    None b -> selector b
    Deep     None b -> selector b
    Adjacent None b -> selector b
    Combined None b -> selector b
    Child    a    b -> selector a <> " > " <> selector b
    Deep     a    b -> selector a <> " "   <> selector b
    Adjacent a    b -> selector a <> " + " <> selector b
    Combined a    b -> selector a <> " , " <> selector b

property :: (Text, Text) -> Builder
property (key, val) =
  mconcat ["  ", fromText key, ": ", fromText val, ";\n"]

