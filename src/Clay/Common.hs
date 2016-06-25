{-# LANGUAGE OverloadedStrings #-}

-- | A bunch of type classes representing common values shared between multiple
-- CSS properties, like `Auto`, `Inherit`, `None`, `Normal` and several more.
--
-- All the common value type classes have an instance for the Value type,
-- making them easily derivable for custom value types.


module Clay.Common where

import Clay.Property
import Data.String (IsString)
import Data.Monoid ((<>))

-------------------------------------------------------------------------------

class All      a where all      :: a
class Auto     a where auto     :: a
class Baseline a where baseline :: a
class Center   a where center   :: a
class Inherit  a where inherit  :: a
class None     a where none     :: a
class Normal   a where normal   :: a
class Visible  a where visible  :: a
class Hidden   a where hidden   :: a
class Initial  a where initial  :: a
class Unset    a where unset    :: a

-- | The other type class is used to escape from the type safety introduced by
-- embedding CSS properties into the typed world of Clay. `Other` allows you to
-- cast any `Value` to a specific value type.

class Other   a where other   :: Value -> a

instance All      Value where all      = "all"
instance Auto     Value where auto     = "auto"
instance Baseline Value where baseline = "baseline"
instance Center   Value where center   = "center"
instance Inherit  Value where inherit  = "inherit"
instance Normal   Value where normal   = "normal"
instance None     Value where none     = "none"
instance Visible  Value where visible  = "visible"
instance Hidden   Value where hidden   = "hidden"
instance Other    Value where other    = id
instance Initial  Value where initial  = "initial"
instance Unset    Value where unset    = "unset"

-------------------------------------------------------------------------------

-- | Common list browser prefixes to make experimental properties work in
-- different browsers.

browsers :: Prefixed
browsers = Prefixed
  [ ( "-webkit-", "" )
  , (    "-moz-", "" )
  , (     "-ms-", "" )
  , (      "-o-", "" )
  , (         "", "" )
  ]

-------------------------------------------------------------------------------

-- | Syntax for CSS function call.

call :: (IsString s, Monoid s) => s -> s -> s
call fn arg = fn <> "(" <> arg <> ")"

-------------------------------------------------------------------------------

-- | Some auxiliary mathematical functions.

fracMod :: RealFrac a => a -> a -> a
fracMod x y = (x -) . (* y) $ evenMultiples x y
    where evenMultiples x' y' = fromIntegral (truncate (x' / y') :: Integer)

decimalRound :: RealFrac a => a -> Int -> a
decimalRound x decimalPlaces = shiftedAndRounded x / powersOf10
    where powersOf10 = 10 ^ decimalPlaces
          shiftedAndRounded x' = fromIntegral (round $ x' * powersOf10 :: Integer)
