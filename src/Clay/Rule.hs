{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Clay.Rule where

import Data.Text (Text)
import Control.Monad.Writer

import Clay.Selector hiding (Child)
import Clay.Property

data Rule
  = Self     Filter
  | Root     Selector
  | Pop      Int
  | Child    Selector
  | Sub      Selector

newtype Rules = Rules [Either (Key (), Value) (Rule, Rules)]
  deriving Monoid

newtype CssM a = Css (Writer Rules a)
  deriving Monad

type Css = CssM ()

key :: Val a => Key a -> a -> Css
key k v = Css $ tell (Rules [Left (cast k, value v)])

prefixed :: Val a => Prefixed -> a -> Css
prefixed xs = key (Key xs)

infix 4 -:

(-:) :: Key Text -> Text -> Css
(-:) = key

-------------------------------------------------------------------------------

root :: Selector -> Css -> Css
root sel (Css rs) = Css $ tell (Rules [Right (Root sel, execWriter rs)])

pop :: Int -> Css -> Css
pop i (Css rs) = Css $ tell (Rules [Right (Pop i, execWriter rs)])

infixr 5 <?
infixr 5 ?
infixr 5 &

(<?) :: Selector -> Css -> Css
(<?) sel (Css rs) = Css $ tell (Rules [Right (Child sel, execWriter rs)])

(?) :: Selector -> Css -> Css
(?) sel (Css rs) = Css $ tell (Rules [Right (Sub sel, execWriter rs)])

(&) :: Filter -> Css -> Css
(&) p (Css rs) = Css $ tell (Rules [Right (Self p, execWriter rs)])

