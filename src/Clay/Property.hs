{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Clay.Property where

import Data.String
import Data.Text (Text, intercalate)
import Control.Monad.Writer

newtype Key a = Key { unKey :: Text }

instance IsString (Key a) where
  fromString = Key . fromString

newtype Value = Value { unValue :: Text }
  deriving (Monoid, IsString)

class Val a where
  value :: a -> Value

instance Val Text where
  value = Value

instance Val Double where
  value = fromString . show

instance Val Value where
  value = id

instance Val a => Val (Maybe a) where
  value Nothing  = "none"
  value (Just a) = value a

instance (Val a, Val b) => Val (a, b) where
  value (a, b) =
    case (value a, value b) of
      (Value x, Value y) -> Value (x <> " " <> y)

instance Val a => Val [a] where
  value xs = Value (intercalate "," (map (unValue . value) xs))

infixr !

(!) :: a -> b -> (a, b)
(!) = (,)

