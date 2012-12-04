{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Clay.Core.Rule where

import Data.Text
import Control.Monad.Writer

import Clay.Core.Selector hiding (Child)
import Clay.Core.Property

data Rule
  = Self     Filter
  | Root     Selector
  | Pop      Int
  | Child    Selector
  | Sub      Selector

newtype Rules = Rules [Either (Text, Text) (Rule, Rules)]
  deriving Monoid

type Css = Writer Rules ()

key :: Val a => Key a -> a -> Css
key (Key k) v = tell (Rules [Left (k, unValue (value v))])

infix 4 -:

(-:) :: Key Text -> Text -> Css
(-:) = key

-------------------------------------------------------------------------------

root :: Selector -> Css -> Css
root sel rs = tell (Rules [Right (Root sel, execWriter rs)])

pop :: Int -> Css -> Css
pop i rs = tell (Rules [Right (Pop i, execWriter rs)])

infixr 5 <?
infixr 5 ?
infixr 5 &

(<?) :: Selector -> Css -> Css
(<?) sel rs = tell (Rules [Right (Child sel, execWriter rs)])

(?) :: Selector -> Css -> Css
(?) sel rs = tell (Rules [Right (Sub sel, execWriter rs)])

(&) :: Filter -> Css -> Css
(&) p rs = tell (Rules [Right (Self p, execWriter rs)])

