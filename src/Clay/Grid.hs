{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- | Partial implementation of <https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Grid_Layout>.
module Clay.Grid
  ( gap
  , rowGap
  , columnGap
  , gridTemplateRows
  , gridTemplateColumns
  , gridTemplateAreas
  , gridArea
  , GridArea(..)
  , GridTemplateAreas(..)
  , InvalidGridTemplateAreas(..)
  -- deprecated
  , gridGap
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
import Control.Exception (Exception(..), throw)
import Control.Monad (when)


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

newtype Row = Row Int
  deriving Show

data InvalidGridTemplateAreas
  = GridTemplateAreas_Empty
  | GridTemplateAreas_EmptyRow -- Row
  | GridTemplateAreas_NotRectangular -- [Row]
  deriving (Eq, Show)

instance Exception InvalidGridTemplateAreas

instance Val GridTemplateAreas where
  value areas = fromRight' $ do
    let
      wrapInParens text = "\"" <> text <> "\""
      nested = coerce areas :: [[Text]]
      counts = fmap length nested
      longest = maximum counts

    when (length nested == 0) $
      Left GridTemplateAreas_Empty

    when (any (== 0) counts)  $
      Left GridTemplateAreas_EmptyRow

    when (any (/= longest) counts)  $
      Left GridTemplateAreas_NotRectangular

    pure $
      value $
      Text.intercalate "\n" $
      fmap (wrapInParens . Text.intercalate " ") $
      nested


fromRight' :: Either InvalidGridTemplateAreas a -> a
fromRight' = fromRightOrThrow

fromRightOrThrow :: Exception e => Either e a -> a
fromRightOrThrow (Right a) = a
fromRightOrThrow (Left e) = throw e
