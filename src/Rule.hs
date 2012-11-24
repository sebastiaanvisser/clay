{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rule where

import Data.Text
import Control.Monad.Writer

import Selector
import Property

newtype Rules = Rules [Either (Text, Text) (Selector, Rules)]
  deriving (Show, Monoid)

type Css = Writer Rules ()

infix 4 -:
infix 3 ?

key :: Val a => Key a -> a -> Css
key (Key k) v = tell (Rules [Left (k, unValue (value v))])

key2 :: (Val a, Val b) => Key (a, b) -> a -> b -> Css
key2 k a b = key k (a, b)

key3 :: (Val a, Val b, Val c) => Key (a, b, c) -> a -> b -> c -> Css
key3 k a b c = key k (a, b, c)

key4 :: (Val a, Val b, Val c, Val d) => Key (a, b, c, d) -> a -> b -> c -> d -> Css
key4 k a b c d = key k (a, b, c, d)

(-:) :: Val a => Key a -> a -> Css
(-:) = key

(?) :: Selector -> Css -> Css
(?) sel rs = tell (Rules [Right (sel, execWriter rs)])

