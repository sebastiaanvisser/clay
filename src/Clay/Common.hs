{-# LANGUAGE OverloadedStrings #-}

-- | A bunch of type classes representing common values shared between multiple
-- CSS properties, like `Auto`, `Inherit`, `None`, `Normal` and several more.
--
-- All the common value type classes have an instance for the Value type,
-- making them easily derivable for custom value types.


module Clay.Common where

import Clay.Property
import Data.String (IsString)
import Data.Monoid (Monoid, (<>))

-------------------------------------------------------------------------------

class All      a where all      :: a
class Auto     a where auto     :: a
class Baseline a where baseline :: a
class Center   a where center   :: a
class Left     a where left     :: a
class Right    a where right    :: a
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

allValue :: Value
allValue = "all"
autoValue :: Value
autoValue = "auto"
baselineValue :: Value
baselineValue = "baseline"
centerValue :: Value
centerValue = "center"
leftValue :: Value
leftValue = "left"
rightValue :: Value
rightValue = "right"
inheritValue :: Value
inheritValue = "inherit"
normalValue :: Value
normalValue = "normal"
noneValue :: Value
noneValue = "none"
visibleValue :: Value
visibleValue = "visible"
hiddenValue :: Value
hiddenValue = "hidden"
initialValue :: Value
initialValue = "initial"
unsetValue :: Value
unsetValue = "unset"

instance All      Value where all      = allValue
instance Auto     Value where auto     = autoValue
instance Baseline Value where baseline = baselineValue
instance Center   Value where center   = centerValue
instance Left     Value where left     = leftValue
instance Right    Value where right    = rightValue
instance Inherit  Value where inherit  = inheritValue
instance Normal   Value where normal   = normalValue
instance None     Value where none     = noneValue
instance Visible  Value where visible  = visibleValue
instance Hidden   Value where hidden   = hiddenValue
instance Other    Value where other    = id
instance Initial  Value where initial  = initialValue
instance Unset    Value where unset    = unsetValue

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
