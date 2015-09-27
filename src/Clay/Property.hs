{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Clay.Property where

import Control.Arrow (second)
import Control.Monad.Writer
import Data.Fixed (Fixed, HasResolution (resolution), showFixed)
import Data.List (partition, sort)
import Data.Maybe
import Data.String
import Data.Text (Text, replace)

data Prefixed = Prefixed { unPrefixed :: [(Text, Text)] } | Plain { unPlain :: Text }
  deriving (Show, Eq)

instance IsString Prefixed where
  fromString s = Plain (fromString s)

instance Monoid Prefixed where
  mempty  = ""
  mappend = merge

merge :: Prefixed -> Prefixed -> Prefixed
merge (Plain    x ) (Plain    y ) = Plain (x <> y)
merge (Plain    x ) (Prefixed ys) = Prefixed (map (second (x <>)) ys)
merge (Prefixed xs) (Plain    y ) = Prefixed (map (second (<> y)) xs)
merge (Prefixed xs) (Prefixed ys) =
  let kys = map fst ys
      kxs = map fst xs
   in Prefixed $ zipWith (\(p, a) (_, b) -> (p, a <> b))
        (sort (fst (partition ((`elem` kys) . fst) xs)))
        (sort (fst (partition ((`elem` kxs) . fst) ys)))

plain :: Prefixed -> Text
plain (Prefixed xs) = "" `fromMaybe` lookup "" xs
plain (Plain    p ) = p

quote :: Text -> Text
quote t = "\"" <> replace "\"" "\\\"" t <> "\""

-------------------------------------------------------------------------------

newtype Key a = Key { unKeys :: Prefixed }
  deriving (Show, Monoid, IsString)

cast :: Key a -> Key ()
cast (Key k) = Key k

-------------------------------------------------------------------------------

newtype Value = Value { unValue :: Prefixed }
  deriving (Show, Monoid, IsString, Eq)

class Val a where
  value :: a -> Value

instance Val Text where
  value t = Value (Plain t)

newtype Literal = Literal Text
  deriving (Show, Monoid, IsString)

instance Val Literal where
  value (Literal t) = Value (Plain (quote t))

instance Val Integer where
  value = fromString . show

data E5 = E5
instance HasResolution E5 where resolution _ = 100000

instance Val Double where
  value = fromString . showFixed' . realToFrac
    where
      showFixed' :: Fixed E5 -> String
      showFixed' = showFixed True

instance Val Value where
  value = id

instance Val a => Val (Maybe a) where
  value Nothing  = ""
  value (Just a) = value a

instance (Val a, Val b) => Val (a, b) where
  value (a, b) = value a <> " " <> value b

instance (Val a, Val b) => Val (Either a b) where
  value (Left  a) = value a
  value (Right a) = value a

instance Val a => Val [a] where
  value xs = intersperse "," (map value xs)

intersperse :: Monoid a => a -> [a] -> a
intersperse _ []     = mempty
intersperse s (x:xs) = foldl (\a b -> a <> s <> b) x xs

-------------------------------------------------------------------------------

noCommas :: Val a => [a] -> Value
noCommas xs = intersperse " " (map value xs)

infixr !

(!) :: a -> b -> (a, b)
(!) = (,)

