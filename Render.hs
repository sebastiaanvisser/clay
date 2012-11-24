module Render where

import Data.Monoid
import Data.Foldable
import Data.Text.Lazy.Builder

import qualified Data.Text.Lazy.IO as Text

import Rules    (Property (Property), Rule (Rule))
import Selector (Selector (..))


renderAndPrint :: [Rule] -> IO ()
renderAndPrint = Text.putStr . toLazyText . render

render :: [Rule] -> Builder
render = foldMap rule

rule :: Rule -> Builder
rule (Rule sel props) = mconcat
  [ selector sel
  , "\n{\n"
  , foldMap property props
  , "}\n"
  ]

selector :: Selector -> Builder
selector sel =
  case sel of
    None         -> mempty
    Star         -> singleton '*'
    Elem     t   -> fromText t
    Id       t   -> singleton '#' <> fromText t
    Class    t   -> singleton '.' <> fromText t
    Child    a b -> selector a <> " > " <> selector b
    Deep     a b -> selector a <> "   " <> selector b
    Adjacent a b -> selector a <> " + " <> selector b
    Combined a b -> selector a <> " , " <> selector b

property :: Property -> Builder
property (Property key val) =
  fromText key <> ": " <> fromText val <> ";\n"

