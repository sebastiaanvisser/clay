{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  , TypeFamilies
  , RankNTypes
  , ExistentialQuantification
  , PatternSynonyms
  #-}
-- | Implementation of <https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Grid_Layout>.
module Clay.Grid
  ( gap
  , rowGap
  , columnGap
  , gridTemplateRows
  , gridTemplateColumns
  , GridTrackList
  , mkGridTrackList
  , gridAutoRows
  , gridAutoColumns
  , GridAutoTrackList
  , mkGridAutoTrackList
  , gridAutoFlow
  , row
  , column
  , dense
  , rowDense
  , columnDense
  , gridArea
  , blankGridArea
  , GridArea
  , gridRowStart
  , gridRowEnd
  , gridColumnStart
  , gridColumnEnd
  , GridLocation
  , gridLocation
  , IsSpan(..)
  , gridTemplateAreas
  , GridTemplateAreas
  , GridTemplateNamedAreas
  , InvalidGridTemplate(..)
  -- re exports
  , These(..)
  )
  where

import Clay.Common
import Clay.Property
import Clay.Size
import Clay.Stylesheet
import Clay.Elements (span)

import Prelude hiding (span)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Coerce (coerce)
import Data.These
import GHC.Exts (IsList(..))
import Control.Exception (Exception(..), throw)
import Control.Monad (when)


-- | Property sets the gaps (gutters) between rows and columns.
-- Sets both "gap" & "grid-gap"
-- to quote: https://developer.mozilla.org/en-US/docs/Web/CSS/gap
-- "CSS Grid Layout initially defined the grid-gap property. This prefixed property is being replaced by gap. However, in order to support browsers that implemented grid-gap and not gap for grid, you will need to use the prefixed property"
gap :: Size a -> Css
gap = key "gap" <> key "grid-gap"

-- | Property sets the size of the gap (gutter) between an element's grid rows.
-- Sets both "row-gap" & "grid-row-gap"
-- to quote: https://developer.mozilla.org/en-US/docs/Web/CSS/row-gap
-- "CSS Grid Layout initially defined the grid-row-gap property. This prefixed property is being replaced by row-gap. However, in order to support browsers that implemented grid-row-gap and not row-gap for grid, you will need to use the prefixed property."
rowGap :: Size a -> Css
rowGap = key "row-gap" <> key "grid-row-gap"

-- | Property sets the size of the gap (gutter) between an element's grid columns.
-- Sets both "column-gap" & "grid-column-gap"
-- to quote: https://developer.mozilla.org/en-US/docs/Web/CSS/column-gap
-- "CSS Grid Layout initially defined the grid-column-gap property. This prefixed property is being replaced by column-gap. However, in order to support bcolumnsers that implemented grid-column-gap and not column-gap for grid, you will need to use the prefixed property."
columnGap :: Size a -> Css
columnGap = key "column-gap" <> key "grid-column-gap"

-------------------------------------------------------------------------------

-- | Property defines the line names and track sizing functions of the grid rows.
gridTemplateRows :: GridTrackList a -> Css
gridTemplateRows = key "grid-template-rows"

-- | Property defines the line names and track sizing functions of the grid columns.
gridTemplateColumns :: GridTrackList a -> Css
gridTemplateColumns = key "grid-template-columns"

newtype GridTrackList a = GridTrackList Value
  deriving (Val, None, Inherit, Initial, Unset)

-- | Create a GridTrackList from a list of sizes
mkGridTrackList :: [Size a] -> GridTrackList a
mkGridTrackList = GridTrackList . noCommas

-------------------------------------------------------------------------------

-- | Property defines the line names and track sizing functions of the grid rows.
gridAutoRows :: GridAutoTrackList a -> Css
gridAutoRows = key "grid-auto-rows"

-- | Property defines the line names and track sizing functions of the grid columns.
gridAutoColumns :: GridAutoTrackList a -> Css
gridAutoColumns = key "grid-auto-columns"

newtype GridAutoTrackList a = GridAutoTrackList Value
  deriving (Val, Auto, MinContent, MaxContent)

mkGridAutoTrackList :: [Size a] -> GridAutoTrackList
mkGridAutoTrackList = GridAutoTrackList . noCommas

-------------------------------------------------------------------------------
gridAutoFlow :: GridAutoFlow -> Css
gridAutoFlow = key "grid-auto-flow"

newtype GridAutoFlow = GridAutoFlow Value
  deriving (Val, Row, Column, Inherit, Initial, Unset)

dense :: GridAutoFlow
dense = GridAutoFlow "dense"

rowDense :: GridAutoFlow
rowDense = GridAutoFlow "row dense"

columnDense :: GridAutoFlow
columnDense = GridAutoFlow "column dense"
-------------------------------------------------------------------------------
-- | Property defines the element location inside grid template
gridArea :: GridArea -> Css
gridArea = key "grid-area"

blankGridArea :: GridArea
blankGridArea = GridArea "."

newtype GridArea = GridArea Text
  deriving (IsString, Val)

-------------------------------------------------------------------------------

gridRowStart :: GridLocation -> Css
gridRowStart = key "grid-row-start"

gridRowEnd :: GridLocation -> Css
gridRowEnd = key "grid-row-end"

gridColumnStart :: GridLocation -> Css
gridColumnStart = key "grid-column-start"

gridColumnEnd :: GridLocation -> Css
gridColumnEnd = key "grid-column-end"

gridLocation :: IsSpan -> These Integer GridArea -> GridLocation
gridLocation isSpan = GridLocationData . MkGridLocationData isSpan

data IsSpan = Span | NoSpan
  deriving (Show, Eq)

data GridLocation
  = GridLocationKeyword Value
  | GridLocationData GridLocationData

instance Val GridLocation where
  value (GridLocationKeyword v) = v
  value (GridLocationData d)    = value d

instance Auto    GridLocation where auto    = GridLocationKeyword auto
instance Inherit GridLocation where inherit = GridLocationKeyword inherit
instance Initial GridLocation where initial = GridLocationKeyword initial
instance Unset   GridLocation where unset   = GridLocationKeyword unset

-- See under syntax:
-- https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column-start
-- either grid index and/or named grid area is required, but span is optional
-- Note: constructor is not exported, 'GridIndex, or 'gridLocationData to fill this info in
data GridLocationData = MkGridLocationData IsSpan (These Integer GridArea)


instance Val GridLocationData where
  value (MkGridLocationData isSpan indexAndOrGridArea) =
    if isSpan == Span
    then value (span :: Value, indexAndOrGridArea)
    else value indexAndOrGridArea

pattern GridIndex :: Integer -> GridLocation
pattern GridIndex n = GridLocationData (MkGridLocationData NoSpan (This n))

-------------------------------------------------------------------------------

-- | Property defines the template for grid layout
gridTemplateAreas :: GridTemplateAreas -> Css
gridTemplateAreas = key "grid-template-areas"

newtype GridTemplateAreas = GridTemplateAreas Value
  deriving (Val, None, Inherit, Initial, Unset)

instance IsList GridTemplateAreas where
  type Item GridTemplateAreas = [GridArea]
  toList = error "toList GridTemplateAreas is not defined"
  fromList = GridTemplateAreas . value . fromList'
    where
      fromList' :: [Item GridTemplateNamedAreas] -> GridTemplateNamedAreas
      fromList' = fromList

-- have to create a newtype to override the Val instance for lists
newtype GridTemplateNamedAreas = GridTemplateNamedAreas { unGridTemplateNamedAreas :: [[GridArea]] }

instance Val GridTemplateNamedAreas where
  value areas =
    let
      rows = coerce areas :: [[Text]]
      wrapInParens text = "\"" <> text <> "\""
      convertRow = wrapInParens . Text.intercalate " "
    in
      value $
      Text.intercalate "\n" $
      fmap convertRow $
      rows

-- | Smart constructor for GridTemplateNamedAreas
mkGridTemplateNamedAreas :: [[GridArea]] -> Either InvalidGridTemplate GridTemplateNamedAreas
mkGridTemplateNamedAreas rows = do
    let
      counts = fmap length (coerce rows :: [[GridArea]])
      shortestRowLength = minimum counts

    when (null rows) $
      Left InvalidGridTemplateEmpty

    when (shortestRowLength == 0) $
      Left InvalidGridTemplateEmptyRow

    when (any (/= shortestRowLength) counts)  $
      Left InvalidGridTemplateNotRectangular

    Right $ GridTemplateNamedAreas rows


-- | Possible failure modes for 'mkGridTemplateNamedAreas
data InvalidGridTemplate
  = InvalidGridTemplateEmpty
  | InvalidGridTemplateEmptyRow
  | InvalidGridTemplateNotRectangular
  deriving (Eq, Show)
