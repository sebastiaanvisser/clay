{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- | Partial implementation of <https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Grid_Layout>.
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
  ( gap
  , gridGap
  , rowGap
  , columnGap
  , gridTemplateRows
  , gridTemplateColumns
  , gridTemplateAreas
  , gridArea
  , GridArea(..)
  , GridTemplateAreas(..)
  )
  where

import Clay.Property
import Clay.Size
import Clay.Stylesheet

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Coerce (coerce)
import GHC.Exts (IsList(..))


-- | Property sets the gaps (gutters) between rows and columns.
gap :: Size a -> Size a -> Css
gap row col = key "gap" (row, col) <> key "grid-gap" (row, col)

gridGap :: Size a -> Css
gridGap = key "grid-gap"
{-# DEPRECATED gridGap "Use gap, rowGap, and/or columnGap instead" #-}

-- | Property sets the size of the gap (gutter) between an element's grid rows.
rowGap :: Size a -> Css
rowGap = key "row-gap" <> key "grid-row-gap"

-- | Property sets the size of the gap (gutter) between an element's grid columns.
columnGap :: Size a -> Css
columnGap = key "column-gap" <> key "grid-column-gap"

-- | Property defines the line names and track sizing functions of the grid rows.
gridTemplateRows :: [Size a] -> Css
gridTemplateRows = key "grid-template-rows" . noCommas

-- | Property defines the line names and track sizing functions of the grid columns.
gridTemplateColumns :: [Size a] -> Css
gridTemplateColumns = key "grid-template-columns" . noCommas

-- | Property defines the template for grid layout
gridTemplateAreas :: GridTemplateAreas -> Css
gridTemplateAreas = key "grid-template-areas"

-- | Property defines the element location inside grid template
gridArea :: GridArea -> Css
gridArea = key "grid-area"

newtype GridArea = GridArea Text
  deriving (IsString, Val)

-- have to create a newtype to override the Val instance for lists
newtype GridTemplateAreas = GridTemplateAreas { unGridTemplateAreas :: [[GridArea]] }

instance IsList GridTemplateAreas where
  type Item GridTemplateAreas = [GridArea]
  fromList = GridTemplateAreas
  toList = unGridTemplateAreas

instance Val GridTemplateAreas where
  value areas =
    value $
    Text.intercalate "\n" $
    fmap (quote . Text.intercalate " ") $
    (coerce areas :: [[Text]])
    where
      quote text = "\"" <> text <> "\""
