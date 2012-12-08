{-# LANGUAGE OverloadedStrings #-}
module Clay.Common where

import Clay.Property

-------------------------------------------------------------------------------

-- | A bunch of type classes representing common values shared between multiple
-- CSS properties.

class Auto    a where auto    ::          a
class Inherit a where inherit ::          a
class None    a where none    ::          a
class Normal  a where normal  ::          a
class Visible a where visible ::          a
class Hidden  a where hidden  ::          a
class Other   a where other   :: Value -> a

-- | All the common value type classes have an instance for the Value type,
-- making them easily derivable for custom value types.

instance Auto    Value where auto    = "auto"
instance Inherit Value where inherit = "inherit"
instance Normal  Value where normal  = "normal"
instance None    Value where none    = "none"
instance Visible Value where visible = "visible"
instance Hidden  Value where hidden  = "hidden"
instance Other   Value where other   = id

-------------------------------------------------------------------------------

browsers :: Prefixed
browsers = Prefixed
  [ ( "-webkit-", "" )
  , (    "-moz-", "" )
  , (     "-ms-", "" )
  , (      "-o-", "" )
  , (         "", "" )
  ]

