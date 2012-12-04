{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Clay.Core.Property where

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

instance (Val a, Val b, Val c) => Val (a, b, c) where
  value (a, b, c) = value (a, (b, c))

instance (Val a, Val b, Val c, Val d) => Val (a, b, c, d) where
  value (a, b, c, d) = value (a, (b, (c, d)))

instance (Val a, Val b, Val c, Val d, Val e) => Val (a, b, c, d, e) where
  value (a, b, c, d, e) = value (a, (b, (c, (d, e))))

instance (Val a, Val b, Val c, Val d, Val e, Val f) => Val (a, b, c, d, e, f) where
  value (a, b, c, d, e, f) = value (a, (b, (c, (d, (e, f)))))

instance (Val a, Val b, Val c, Val d, Val e, Val f, Val g) => Val (a, b, c, d, e, f, g) where
  value (a, b, c, d, e, f, g) = value (a, (b, (c, (d, (e, (f, g))))))

instance (Val a, Val b, Val c, Val d, Val e, Val f, Val g, Val h) => Val (a, b, c, d, e, f, g, h) where
  value (a, b, c, d, e, f, g, h) = value (a, (b, (c, (d, (e, (f, (g, h)))))))

instance Val a => Val [a] where
  value xs = Value (intercalate "," (map (unValue . value) xs))

