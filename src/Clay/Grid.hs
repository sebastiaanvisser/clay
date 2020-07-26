{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Partial implementation of <https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Grid_Layout>.
module Clay.Grid
  ( gap
  , rowGap
  , columnGap
  , gridTemplateRows
  , gridTemplateColumns
  , GridTrackList
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
  , mkGridTemplateNamedAreas
  , unGridTemplateNamedAreas
  , InvalidGridTemplateNamedAreas(..)
  -- re exports
  , These(..)
  -- deprecated
  , gridGap

  )
  where

import Clay.Common
import Clay.Property
import Clay.Size
import Clay.Stylesheet

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Coerce (coerce)
import Data.These
import GHC.Exts (IsList(..))
import Control.Exception (Exception(..), throw)
import Control.Monad (when)


-- | Property sets the gaps (gutters) between rows and columns.
gap :: Size a -> Css
gap = key "gap" <> key "grid-gap"

gridGap :: Size a -> Css
gridGap = gap
{-# DEPRECATED gridGap "Use gap, rowGap, and/or columnGap instead" #-}

-- | Property sets the size of the gap (gutter) between an element's grid rows.
rowGap :: Size a -> Css
rowGap = key "row-gap" <> key "grid-row-gap"

-- | Property sets the size of the gap (gutter) between an element's grid columns.
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

instance IsList (GridTrackList a) where
  type Item (GridTrackList a) = Size a
  toList = error ""
  fromList = GridTrackList . noCommas

-------------------------------------------------------------------------------

-- | Property defines the line names and track sizing functions of the grid rows.
gridAutoRows :: GridAutoTrackList a -> Css
gridTemplateRows = key "grid-template-rows"

-- | Property defines the line names and track sizing functions of the grid columns.
gridAutoColumns :: GridAutoTrackList a -> Css
gridTemplateColumns = key "grid-template-columns"

newtype GridAutoTrackList a = GridAutoTrackList Value
  deriving (Val, Auto, MinContent, MaxContent, Inherit, Initial, Unset)

instance IsList (GridAutoTrackList a) where
  type Item (GridAutoTrackList a) = Size a
  toList = error ""
  fromList = GridAutoTrackList . noCommas


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
gridLocation isSpan these = GridLocation_Data $ GridLocationData isSpan these

data IsSpan = Span | NoSpan
  deriving (Show, Eq)

data GridLocation
  = GridLocation_Keyword Value
  | GridLocation_Data GridLocationData

instance Val GridLocation where
  value (GridLocation_Keyword v) = v
  value (GridLocation_Data d) = value d

instance Auto    GridLocation where auto    = GridLocation_Keyword auto
instance Inherit GridLocation where inherit = GridLocation_Keyword inherit
instance Initial GridLocation where initial = GridLocation_Keyword initial
instance Unset   GridLocation where unset   = GridLocation_Keyword unset

data GridLocationData = GridLocationData
  { gridLocation_span                    :: IsSpan
  , gridLocation_coordinateAndOrGridArea :: These Integer GridArea
  }

instance (Val a, Val b) => Val (These a b) where
  value (This a) = value a
  value (That b) = value b
  value (These a b) = value (a, b)

instance Val GridLocationData where
  value (GridLocationData isSpan coordinateAndOrGridArea) =
    if isSpan == Span
    then value ("span" :: Text, coordinateAndOrGridArea)
    else value coordinateAndOrGridArea

pattern GridIndex :: Integer -> GridLocation
pattern GridIndex n = GridLocation_Data (GridLocationData NoSpan (This n))

instance Num GridLocation where
  fromInteger = GridIndex
  negate (GridIndex index) = GridIndex $ negate index

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

-- | toList will throw when your grid template areas are invalid
instance IsList GridTemplateNamedAreas where
  type Item GridTemplateNamedAreas = [GridArea]
  toList = unGridTemplateNamedAreas . coerce
  fromList = fromRightOrThrow . mkGridTemplateNamedAreas
    where
      fromRightOrThrow :: Exception e => Either e a -> a
      fromRightOrThrow (Right a) = a
      fromRightOrThrow (Left e) = throw e

-- | Smart constructor for GridTemplateNamedAreas
mkGridTemplateNamedAreas :: [[GridArea]] -> Either InvalidGridTemplateNamedAreas GridTemplateNamedAreas
mkGridTemplateNamedAreas rows = do
    let
      counts = fmap length (coerce rows :: [[GridArea]])
      longest = maximum counts

    when (null rows ) $
      Left GridTemplateNamedAreas_Empty

    when (any (== 0) counts)  $
      Left GridTemplateNamedAreas_EmptyRow

    when (any (/= longest) counts)  $
      Left GridTemplateNamedAreas_NotRectangular

    Right $ GridTemplateNamedAreas rows

-- | Failure modes for the smart constructor
data InvalidGridTemplateNamedAreas
  = GridTemplateNamedAreas_Empty
  | GridTemplateNamedAreas_EmptyRow
  | GridTemplateNamedAreas_NotRectangular
  deriving (Eq, Show)

instance Exception InvalidGridTemplateNamedAreas
------------------------------------------------------------------------------
