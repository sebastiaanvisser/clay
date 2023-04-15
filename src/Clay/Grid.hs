{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | Partial implementation of <https://alligator.io/css/css-grid-layout-grid-areas grid area CSS API>.
--
-- For instance, you want to generate the following CSS:
--
-- @
-- .grid1 {
--   display: grid;
--   width: max-content;
-- }
--
-- .grid3 {
--   display: grid;
--   width: max-content;
-- }
--
-- \@media (min-width: 40.0rem) {
--   .grid3 {
--     display: grid;
--     grid-template-columns: 1fr 1fr 1fr;
--     grid-gap: 1rem;
--     width: max-content;
--   }
-- }
-- @
--
-- The corresponding clay code:
--
-- @
--  ".grid1" ? do
--    display grid
--    width maxContent
--  ".grid3" ? do
--    display grid
--    width maxContent
--  query M.screen [M.minWidth (rem 40)] $ ".grid3" ? do
--    display grid
--    gridTemplateColumns [fr 1, fr 1, fr 1]
--    gridGap $ rem 1
--    width maxContent
-- @
module Clay.Grid
(
    -- * Grid
    gridGap
  , gridTemplateColumns

    -- * Size and location

    -- ** Data types and type classes
  , GridLine (..)
  , ToGridLine

    -- ** Style properties
  , gridArea
  , gridColumn
  , gridColumnStart
  , gridColumnEnd
  , gridRow
  , gridRowStart
  , gridRowEnd

    -- ** Keywords
  , (//)
  , span_
)
where

import qualified Clay.Common as Com
import           Clay.Property (Val, noCommas, value)
import           Clay.Size (Size)
import           Clay.Stylesheet (Css, key)
import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude hiding (span)

-- | Property sets the gaps (gutters) between rows and columns.
gridGap :: Size a -> Css
gridGap = key "grid-gap"

-- | Property defines the line names and track sizing functions of the grid columns.
gridTemplateColumns :: [Size a] -> Css
gridTemplateColumns = key "grid-template-columns" . noCommas

-- | A `grid-line` value.
--
-- A grid-line value specifies a size and location in a grid.
-- the following are invalid:
--
-- NOTE: although you can use the below constructors, it's also possible
-- to use a closer CSS syntax taking advantage of the 'ToGridLine' instances.
data GridLine

  -- | 'Integer' value.
  --
  -- NOTE: 'Integer' value of 0 is invalid.
  = Coordinate Integer

  -- | `custom-ident` with an optional 'Integer' value.
  --
  -- NOTE: 'Integer' value of 0 is invalid.
  | CustomIndent String (Maybe Integer)

  -- | `span` CSS keyword with an optional `custom-ident` and/or 'Integer' value.
  --
  -- NOTE: negative 'Integer' or 0 are invalid.
  | Span (Maybe String) (Maybe Integer)

  -- | `auto` CSS keyword.
  | Auto

  -- | `inherit` CSS keyword.
  | Inherit

  -- | `initial` CSS keyword.
  | Initial

  -- | `unset` CSS keyword.
  | Unset
  deriving (Eq, Show)

class ToGridLine a where
  -- | Convert the provided type to a 'GridLine'.
  toGridLine :: a -> GridLine

instance ToGridLine GridLine where
  toGridLine = id

instance ToGridLine Integer where
  -- | NOTE: 'Integer' value of 0 is invalid.
  toGridLine = Coordinate

-- | `custom-ident` value.
instance ToGridLine String where
  toGridLine x = CustomIndent x Nothing

-- | Both `custom-ident` and `Integer` values, provided as a pair.
--
-- NOTE: 'Integer' value of 0 is invalid.
instance ToGridLine (String, Integer) where
  toGridLine (x, y) = CustomIndent x (Just y)

-- | One or two `grid-line` values.
--
-- NOTE: although you can use the below constructors, it's also possible
-- to use a closer CSS syntax using the 'ToGridLines2' instances.
data GridLines2
    -- | One `grid-line` value.
  = One2 OneGridLine
    -- | Two `grid-line` values.
  | Two2 TwoGridLines

class ToGridLines2 a where

  -- | Convert the provided type to 'GridLines2' (one or two `grid-line` values).
  toGridLines2 :: a -> GridLines2

instance ToGridLines2 GridLine where
  -- | One `grid-line` value.
  toGridLines2 = One2 . OneGridLine

instance ToGridLines2 OneGridLine where
  -- | One `grid-line` value.
  toGridLines2 = One2

instance ToGridLines2 TwoGridLines where
  -- | Two `grid-line` values.
  toGridLines2 = Two2

instance ToGridLines2 GridLines2 where
  -- | One or two `grid-line` values.
  toGridLines2 = id

instance ToGridLines2 Integer where
  -- | One 'Integer' value.
  --
  -- NOTE: 'Integer' value of 0 is invalid.
  toGridLines2 = toGridLines2 . toGridLine

instance ToGridLines2 String where
  -- | One `custom-ident` value.
  toGridLines2 = toGridLines2 . toGridLine

instance ToGridLines2 (String, Integer) where
  -- | One time both a `custom-ident` and 'Integer' values, provided as a pair.
  --
  -- NOTE: 'Integer' value of 0 is invalid.
  toGridLines2 = toGridLines2 . toGridLine

-- | One, two, three or four `grid-line` values.
--
-- NOTE: although you can use the below constructors, it's also possible
-- to use a closer CSS syntax using the 'ToGridLines4' instances.
data GridLines4

    -- | One `grid-line` value.
  = One4 OneGridLine

    -- | Two `grid-line` values.
  | Two4 TwoGridLines

    -- | Three `grid-line` values.
  | Three4 ThreeGridLines

    -- | Four `grid-line` values.
  | Four4 FourGridLines

class ToGridLines4 a where
  -- | Convert the provided type to 'GridLines4'
  -- (one, two, three or four `grid-line` values).
  toGridLines4 :: a -> GridLines4

instance ToGridLines4 GridLine where
  -- | One `grid-line` value.
  toGridLines4 = One4 . OneGridLine

instance ToGridLines4 OneGridLine where
  -- | One `grid-line` value.
  toGridLines4 = One4

instance ToGridLines4 TwoGridLines where
  -- | Two `grid-line` values.
  toGridLines4 = Two4

instance ToGridLines4 ThreeGridLines where
  -- | Three `grid-line` values.
  toGridLines4 = Three4

instance ToGridLines4 FourGridLines where
  -- | Four `grid-line` values.
  toGridLines4 = Four4

instance ToGridLines4 GridLines4 where
  -- | One, two, three or four `grid-line` values.
  toGridLines4 = id

instance ToGridLines4 Integer where
  -- | One 'Integer' value.
  toGridLines4 = toGridLines4 . toGridLine

instance ToGridLines4 String where
  -- | One `custom-ident` value.
  toGridLines4 = toGridLines4 . toGridLine

instance ToGridLines4 (String, Integer) where
  -- | One time both a `custom-ident` and 'Integer' values, provided as a pair.
  toGridLines4 = toGridLines4 . toGridLine

-- | One 'GridLine' value.
newtype OneGridLine = OneGridLine GridLine

-- | Two 'GridLine' values.
data TwoGridLines = TwoGridLines GridLine GridLine

-- | Three 'GridLine' values.
data ThreeGridLines = ThreeGridLines GridLine GridLine GridLine

-- | Four 'GridLine' values.
data FourGridLines = FourGridLines GridLine GridLine GridLine GridLine

-- | Property shorthand specifies a grid item's size and location
-- within a grid.
--
-- One to four `grid-line` values can be specified.
-- Grid-line values must be separated by a '(//)' operator.
--
-- WARNING: this function is partial.
--
--
-- ==== __Examples__
--
-- >>> gridArea 3
--
-- >>> gridArea2 $ 2 // "nav"
--
-- >>> gridArea $ ("nav", 2) // span_ 3 // 4
gridArea :: ToGridLines4 a => a -> Css
gridArea x = key "grid-area" (partialToGridLines4 x)

-- | Property shorthand specifies a grid item's size and location
-- within a grid column.
--
-- TODO write doc.
gridColumn :: ToGridLines2 a => a -> Css
gridColumn x = key "grid-column" (partialToGridLines2 x)

-- | Property specifies a grid item's start position within the grid column.
--
-- TODO: write doc.
gridColumnStart :: ToGridLine a => a -> Css
gridColumnStart x = key "grid-column-start" (partialToGridLine x)

-- | Property specifies a grid item's end position within the grid column.
--
-- TODO: write doc.
gridColumnEnd :: ToGridLine a => a -> Css
gridColumnEnd x = key "grid-column-end" (partialToGridLine x)

-- | Property shorthand specifies a grid item's size and location
-- within a grid row.
--
-- One or two `grid-line` values can be specified.
-- `grid-line` values must be separated by a '(//)' operator.
--
-- WARNING: this function is partial, see above documentation.
--
-- ==== __Examples__
--
-- >>> gridRow "nav"
--
-- >>> gridRow $ 3 // span_ 2
gridRow :: ToGridLines2 a => a -> Css
gridRow x = key "grid-row" (partialToGridLines2 x)

-- | Property specifies a grid item's start position within the grid row.
--
-- WARNING: this function is partial, see above documentation.
-- TODO: move this up in a global comment.
-- - an 'Integer' value of 0
-- - a pair with an 'Integer' component of value 0
-- - a 'span_' function provided with an 'Integer' value of 0 or negative
-- - a 'span_' function provided with a pair value with
-- an 'Integer' component of 0 or negative.
-- - a 'GridLine' value representing one of the above.
gridRowStart :: ToGridLine a => a -> Css
gridRowStart x = key "grid-row-start" (partialToGridLine x)

-- | Property specifies a grid item's end position within the grid row.
--
-- WARNING: this function is partial, see above documentation.
gridRowEnd :: ToGridLine a => a -> Css
gridRowEnd x = key "grid-row-end" (partialToGridLine x)

class Slash a r | a -> r where
  -- | `/` CSS operator.
  -- Separates `grid-line` values.
  (//) :: ToGridLine b => a -> b -> r

instance Slash GridLine TwoGridLines where
  x // y = TwoGridLines x (toGridLine y)

instance Slash Integer TwoGridLines where
  x // y = TwoGridLines (toGridLine x) (toGridLine y)

instance Slash String TwoGridLines where
  x // y = TwoGridLines (toGridLine x) (toGridLine y)

instance Slash (String, Integer) TwoGridLines where
  x // y = TwoGridLines (toGridLine x) (toGridLine y)

instance Slash TwoGridLines ThreeGridLines where
  (TwoGridLines xx xy) // y = ThreeGridLines xx xy (toGridLine y)

instance Slash ThreeGridLines FourGridLines where
  (ThreeGridLines xx xy xz) // y = FourGridLines xx xy xz (toGridLine y)

class ToSpan a where

  -- | Contributes to the grid item's placement.
  span_ :: a -> GridLine

instance ToSpan Integer where

  -- | Contributes the nth grid line to the grid item's placement.
  --
  -- NOTE: negative 'Integer' or 0 values are invalid.
  span_ x = Span Nothing (Just x)

instance ToSpan String where

  -- | One line from the provided name is counted.
  span_ x = Span (Just x) Nothing

instance ToSpan (String, Integer) where

  -- | Nth lines from the provided name are counted.
  --
  -- NOTE: negative 'Integer' or 0 values are invalid.
  span_ (x, y) = Span (Just x) (Just y)

-- | Keyword indicating that the property contributes nothing to the grid item's
-- placement.
instance Com.Auto GridLine where
  auto = Auto

-- | Keyword `inherit` applied to a 'GridLine'.
instance Com.Inherit GridLine where
  inherit = Inherit

-- | Keyword `initial` applied to a 'GridLine'.
instance Com.Initial GridLine where
  initial = Initial

-- | Keyword `unset` applied to a 'GridLine'.
instance Com.Unset GridLine where
  unset = Unset

-- | Convertion of 'GridLine' to 'Clay.Property.Value'.
instance Val GridLine where
  value Auto               = "auto"
  value Inherit            = "inherit"
  value Initial            = "initial"
  value Unset              = "unset"
  value (Coordinate x)     = value x
  value (CustomIndent x y) = value $ T.pack x <> foldMap ((" " <>) . tshow) y
  value (Span x y) =
    value $ "span" <> foldMap ((" " <>) . T.pack) x <> foldMap ((" " <>) . tshow) y

instance Val OneGridLine where
  value (OneGridLine x) = value x

instance Val TwoGridLines where
  value (TwoGridLines x y) =
    value x <> value (" / " :: Text) <> value y

instance Val ThreeGridLines where
  value (ThreeGridLines x y z) =
       value x
    <> value (" / " :: Text) <> value y
    <> value (" / " :: Text) <> value z

instance Val FourGridLines where
  value (FourGridLines xx xy xz yx) =
       value xx
    <> value (" / " :: Text) <> value xy
    <> value (" / " :: Text) <> value xz
    <> value (" / " :: Text) <> value yx

instance Val GridLines2 where
  value (One2 x) = value x
  value (Two2 x) = value x

instance Val GridLines4 where
  value (One4 x)   = value x
  value (Two4 x)   = value x
  value (Three4 x) = value x
  value (Four4 x)  = value x

-- | Private partial function checking a 'GridLine'.
--
-- An error is raised when:
-- - An 'Integer' value of 0 is provided.
-- - A negative 'Integer' value is provided for a 'Span' constructor.
--
-- Otherwise, the initially provided 'GridLine' value is returned.
partialCheckGridLine :: GridLine -> GridLine
partialCheckGridLine gridLine = case gridLine of
  Coordinate 0            -> errorValue 0
  CustomIndent _ (Just 0) -> errorValue 0
  s@(Span _ (Just n))     -> if n < 1
                             then errorValue n
                             else s
  _                       -> gridLine
  where
    errorValue n = error ("Value " ++ show n ++ " is invalid")

-- | Private partial function checking 'OneGridLine'.
--
-- An error is raised if an invalid value is found, see 'partialCheckGridLine'.
partialCheckOneGridLine :: OneGridLine -> OneGridLine
partialCheckOneGridLine (OneGridLine gridLine) =
  OneGridLine (partialCheckGridLine gridLine)

-- | Private partial function checking 'TwoGridLines'.
--
-- An error is raised if an invalid value is found, see 'partialCheckGridLine'.
partialCheckTwoGridLines :: TwoGridLines -> TwoGridLines
partialCheckTwoGridLines (TwoGridLines gridLine1 gridLine2) =
  TwoGridLines (partialCheckGridLine gridLine1) (partialCheckGridLine gridLine2)

-- | Private partial function checking 'ThreeGridLines'.
--
-- An error is raised if an invalid value is found, see 'partialCheckGridLine'.
partialCheckThreeGridLines :: ThreeGridLines -> ThreeGridLines
partialCheckThreeGridLines (ThreeGridLines gridLine1 gridLine2 gridLine3) =
  ThreeGridLines
    (partialCheckGridLine gridLine1)
    (partialCheckGridLine gridLine2)
    (partialCheckGridLine gridLine3)

-- | Private partial function checking 'FourGridLines'.
--
-- An error is raised if an invalid value is found, see 'partialCheckGridLine'.
partialCheckFourGridLines :: FourGridLines -> FourGridLines
partialCheckFourGridLines
  (FourGridLines gridLine1 gridLine2 gridLine3 gridLine4) =
    FourGridLines
      (partialCheckGridLine gridLine1)
      (partialCheckGridLine gridLine2)
      (partialCheckGridLine gridLine3)
      (partialCheckGridLine gridLine4)

-- | Private partial function converting its argument to 'GridLine'.
--
-- An error is raised if an invalid value is found, see 'partialCheckGridLine'.
partialToGridLine :: ToGridLine a => a -> GridLine
partialToGridLine = partialCheckGridLine . toGridLine

-- | Private partial function converting its argument to 'GridLines2'.
--
-- An error is raised if an invalid value is found, see 'partialCheckGridLine'.
partialToGridLines2 :: ToGridLines2 a => a -> GridLines2
partialToGridLines2 x = partialGridLine' gridLines
  where
    gridLines = toGridLines2 x
    partialGridLine' (One2 gl) = One2 (partialCheckOneGridLine gl)
    partialGridLine' (Two2 gl) = Two2 (partialCheckTwoGridLines gl)

-- | Private partial function converting its argument to 'GridLines4'.
--
-- An error is raised if an invalid value is found, see 'partialCheckGridLine'.
partialToGridLines4 :: ToGridLines4 a => a -> GridLines4
partialToGridLines4 x = partialGridLine' gridLines
  where
    gridLines = toGridLines4 x
    partialGridLine' (One4 gl)   = One4 (partialCheckOneGridLine gl)
    partialGridLine' (Two4 gl)   = Two4 (partialCheckTwoGridLines gl)
    partialGridLine' (Three4 gl) = Three4 (partialCheckThreeGridLines gl)
    partialGridLine' (Four4 gl)  = Four4 (partialCheckFourGridLines gl)

-- | Private utility function to show 'Text' instead of 'String'.
tshow :: Show a => a -> Text
tshow = T.pack . show
