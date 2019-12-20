{-# LANGUAGE OverloadedStrings #-}
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
