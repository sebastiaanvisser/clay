{-# LANGUAGE OverloadedStrings #-}

-- | A bunch of type classes representing common values shared between multiple
-- CSS properties, like `Auto`, `Inherit`, `None`, `Normal` and several more.
--
-- All the common value type classes have an instance for the Value type,
-- making them easily derivable for custom value types.


module Clay.Common where

import Data.Text (Text)
import Clay.Property
import Data.String (IsString)

-------------------------------------------------------------------------------

class All        a where all        :: a
class Auto       a where auto       :: a
class Baseline   a where baseline   :: a
class Center     a where center     :: a
class Inherit    a where inherit    :: a
class None       a where none       :: a
class Normal     a where normal     :: a
class Visible    a where visible    :: a
class Hidden     a where hidden     :: a
class Initial    a where initial    :: a
class Unset      a where unset      :: a
class MinContent a where minContent :: a
class MaxContent a where maxContent :: a
class Row        a where row        :: a
class Column     a where column     :: a

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
minContentValue :: Value
minContentValue = "min-content"
maxContentValue :: Value
maxContentValue = "max-content"
rowValue :: Value
rowValue = "row"
columnValue :: Value
columnValue = "column"

instance All        Value where all        = allValue
instance Auto       Value where auto       = autoValue
instance Baseline   Value where baseline   = baselineValue
instance Center     Value where center     = centerValue
instance Inherit    Value where inherit    = inheritValue
instance Normal     Value where normal     = normalValue
instance None       Value where none       = noneValue
instance Visible    Value where visible    = visibleValue
instance Hidden     Value where hidden     = hiddenValue
instance Other      Value where other      = id
instance Initial    Value where initial    = initialValue
instance Unset      Value where unset      = unsetValue
instance MinContent Value where minContent = minContentValue
instance MaxContent Value where maxContent = maxContentValue
instance Row        Value where row        = rowValue
instance Column     Value where column     = columnValue

-------------------------------------------------------------------------------

-- | Common list browser prefixes to make experimental properties work in
-- different browsers.

webkitPrefix :: (Text, Text)
webkitPrefix = ( "-webkit-", "" )

emptyPrefix :: (Text, Text)
emptyPrefix = ( "", "" )

webkit :: Prefixed
webkit = Prefixed $
  [ webkitPrefix
  , emptyPrefix
  ]

browsers :: Prefixed
browsers = Prefixed $
  [ webkitPrefix
  , ( "-moz-", "" )
  , (  "-ms-", "" )
  , (   "-o-", "" )
  , emptyPrefix
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
