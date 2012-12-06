{-# LANGUAGE OverloadedStrings #-}
module Clay.Common where

import Clay.Property
import Clay.Rule
import Clay.Size

-------------------------------------------------------------------------------

class Auto    a where auto    ::          a
class Inherit a where inherit ::          a
class None    a where none    ::          a
class Normal  a where normal  ::          a
class Other   a where other   :: Value -> a

instance Auto    Value where auto    = "auto"
instance Inherit Value where inherit = "inherit"
instance Normal  Value where normal  = "normal"
instance None    Value where none    = "none"
instance Other   Value where other   = id

instance Auto    (Size a) where auto    = Size auto
instance Normal  (Size a) where normal  = Size normal
instance Inherit (Size a) where inherit = Size inherit
instance None    (Size a) where none    = Size "0"
instance Other   (Size a) where other   = Size . other

-------------------------------------------------------------------------------

sym :: (Size a -> Size a -> Size a -> Size a -> Css) -> Size a -> Css
sym k a = k a a a a

sym3 :: (Size a -> Size a -> Size a -> Size a -> Css) -> Size a -> Size a -> Size a -> Css
sym3 k tb l r = k tb l tb r

sym2 :: (Size a -> Size a -> Size a -> Size a -> Css) -> Size a -> Size a -> Css
sym2 k tb lr = k tb lr tb lr

