{-# LANGUAGE OverloadedStrings #-}
module Clay.Grid
( gridGap
, gridTemplateColumns
)
where

import Clay.Property
import Clay.Size
import Clay.Stylesheet

-- | Property sets the gaps (gutters) between rows and columns.
gridGap :: Size a -> Css
gridGap = key "grid-gap"

-- | Property defines the line names and track sizing functions of the grid columns.
gridTemplateColumns :: [Size a] -> Css
gridTemplateColumns = key "grid-template-columns" . noCommas
