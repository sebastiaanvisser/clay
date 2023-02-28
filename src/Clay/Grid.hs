{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
(   -- * Style properties.
    gridGap
  , gridTemplateColumns
  , gridArea
  , gridColumn
  , gridColumnStart
  , gridColumnEnd
  , gridRow
  , gridRowStart
  , gridRowEnd

    -- * Keywords
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

-- | Property shorthand specifies a grid item's size and location
-- within a grid.
gridArea :: ToGridLines4 a => a -> Css
gridArea x = key "grid-area" (toGridLines x)

-- | Property shorthand specifies a grid item's size and location
-- within a grid column.
gridColumn :: ToGridLines2 a => a -> Css
gridColumn x = key "grid-column" (toGridLines2 x)

-- | Property specifies a grid item's start position within the grid column.
gridColumnStart :: ToGridLine a => a -> Css
gridColumnStart x = key "grid-column-start" (toGridLine x)

-- | Property specifies a grid item's end position within the grid column.
gridColumnEnd :: ToGridLine a => a -> Css
gridColumnEnd x = key "grid-column-end" (toGridLine x)

-- | Property shorthand specifies a grid item's size and location
-- within a grid row.
gridRow :: ToGridLines2 a => a -> Css
gridRow x = key "grid-row" (toGridLines2 x)

-- | Property specifies a grid item's start position within the grid row.
gridRowStart :: ToGridLine a => a -> Css
gridRowStart x = key "grid-row-start" (toGridLine x)

-- | Property specifies a grid item's end position within the grid row.
gridRowEnd :: ToGridLine a => a -> Css
gridRowEnd x = key "grid-row-end" (toGridLine x)

class Slash a r | a -> r where
  -- | CSS equivalent /.
  -- Separates `GridLine` values.
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

-- | Keyword indicating that the property contributes nothing to the grid item's
-- placement.
instance Com.Auto GridLine where
  auto = Auto

class ToSpan a where

  -- | Contributes to the grid item's placement.
  span_ :: a -> GridLine

instance ToSpan Integer where

  -- | Contributes the nth grid line to the grid item's placement.
  span_ x = Span Nothing (Just x)

instance ToSpan String where

  -- | One line from the provided name is counted.
  span_ x = Span (Just x) Nothing

instance ToSpan (String, Integer) where

  -- | Nth lines from the provided name are counted.
  span_ (x, y) = Span (Just x) (Just y)

-- | Keyword inherit applied to a `GridLine`.
instance Com.Inherit GridLine where
  inherit = Inherit

-- | Keyword initial applied to a `GridLine`.
instance Com.Initial GridLine where
  initial = Initial

-- | Keyword unset applied to a `GridLine`.
instance Com.Unset GridLine where
  unset = Unset

-- | Grid line value.
data GridLine =
    Auto
  | Inherit
  | Initial
  | Unset
  | Coordinate Integer
  | CustomIndent String (Maybe Integer)
  | Span (Maybe String) (Maybe Integer)
  deriving (Eq, Show)

class ToGridLine a where
  toGridLine :: a -> GridLine

instance ToGridLine GridLine where
  toGridLine = id

instance ToGridLine Integer where
  toGridLine = Coordinate

instance ToGridLine String where
  toGridLine x = CustomIndent x Nothing

instance ToGridLine (String, Integer) where
  toGridLine (x, y) = CustomIndent x (Just y)

-- | One or two `GridLine` values.
data GridLines2 =
    One2 OneGridLine
  | Two2 TwoGridLines

-- | One, two, three or four `GridLine` values.
data GridLines4 =
    One OneGridLine
  | Two TwoGridLines
  | Three ThreeGridLines
  | Four FourGridLines

-- | One `GridLine` value
newtype OneGridLine = OneGridLine GridLine

-- | Two `GridLine` values.
data TwoGridLines = TwoGridLines GridLine GridLine

-- | Three `GridLine` values.
data ThreeGridLines = ThreeGridLines GridLine GridLine GridLine

-- | Four `GridLine` values.
data FourGridLines = FourGridLines GridLine GridLine GridLine GridLine

instance Val GridLine where
  value Auto               = "auto"
  value Inherit            = "inherit"
  value Initial            = "initial"
  value Unset              = "unset"
  value (Coordinate x)     = value x
  value (CustomIndent x y) = value $ T.pack x <> foldMap ((" " <>) . tshow) y
  value (Span x y) =
    value $ "span" <> foldMap ((" " <>) . T.pack) x <> foldMap ((" " <>) . tshow) y

-- | Convert a value to `GridLines2` (one or two `GridLine`).
class ToGridLines2 a where
  toGridLines2 :: a -> GridLines2

instance ToGridLines2 GridLines2 where
  toGridLines2 = id

instance ToGridLines2 GridLine where
  toGridLines2 = One2 . OneGridLine

instance ToGridLines2 OneGridLine where
  toGridLines2 = One2

instance ToGridLines2 TwoGridLines where
  toGridLines2 = Two2

instance ToGridLines2 Integer where
  toGridLines2 = toGridLines2 . toGridLine

instance ToGridLines2 String where
  toGridLines2 = toGridLines2 . toGridLine

instance ToGridLines2 (String, Integer) where
  toGridLines2 = toGridLines2 . toGridLine

-- | Convert a value to `GridLines4` (one, two, three or four `GridLine`).
class ToGridLines4 a where
  toGridLines :: a -> GridLines4

instance ToGridLines4 GridLines4 where
  toGridLines = id

instance ToGridLines4 GridLine where
  toGridLines = One . OneGridLine

instance ToGridLines4 OneGridLine where
  toGridLines = One

instance ToGridLines4 TwoGridLines where
  toGridLines = Two

instance ToGridLines4 ThreeGridLines where
  toGridLines = Three

instance ToGridLines4 FourGridLines where
  toGridLines = Four

instance ToGridLines4 Integer where
  toGridLines = toGridLines . toGridLine

instance ToGridLines4 String where
  toGridLines = toGridLines . toGridLine

instance ToGridLines4 (String, Integer) where
  toGridLines = toGridLines . toGridLine

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
  value (One x)   = value x
  value (Two x)   = value x
  value (Three x) = value x
  value (Four x)  = value x

-- | Utility function to show Text instead of String.
tshow :: Show a => a -> Text
tshow = T.pack . show
