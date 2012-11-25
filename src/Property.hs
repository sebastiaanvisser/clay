{-# LANGUAGE OverloadedStrings #-}
module Property where

import Data.String
import Data.Text (Text, intercalate)
import Control.Monad.Writer

-------------------------------------------------------------------------------

newtype Key a = Key { unKey :: Text }

instance IsString (Key a) where
  fromString = Key . fromString

-------------------------------------------------------------------------------

newtype Value = Value { unValue :: Text }

class Val a where
  value :: a -> Value

instance IsString Value where
  fromString = Value . fromString

instance Val Text where
  value = Value

instance (Val a, Val b) => Val (a, b) where
  value (a, b) =
    case (value a, value b) of
      (Value x, Value y) -> Value (x <> " " <> y)

instance (Val a, Val b, Val c) => Val (a, b, c) where
  value (a, b, c) = value (a, (b, c))

instance (Val a, Val b, Val c, Val d) => Val (a, b, c, d) where
  value (a, b, c, d) = value (a, (b, (c, d)))

instance Val a => Val [a] where
  value xs = Value (intercalate ", " (map (unValue . value) xs))

