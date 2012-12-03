module Clay.Style.Common where

import Clay.Core.Property
import Clay.Core.Rule
import Clay.Style.Size

-------------------------------------------------------------------------------

class Auto    a where auto    ::          a
class Inherit a where inherit ::          a
class None    a where none    ::          a
class Other   a where other   :: Value -> a

-------------------------------------------------------------------------------

sym4 :: (Size -> Size -> Size -> Size -> Css) -> Size -> Css
sym4 k a = k a a a a

sym3 :: (Size -> Size -> Size -> Size -> Css) -> Size -> Size -> Size -> Css
sym3 k tb l r = k tb l tb r

sym2 :: (Size -> Size -> Size -> Size -> Css) -> Size -> Size -> Css
sym2 k tb lr = k tb lr tb lr

