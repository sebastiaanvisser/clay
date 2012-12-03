{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Clay.Style.Display where

import Clay.Core.Property
import Clay.Core.Rule
import Clay.Style.Common

newtype Display = Display Value
  deriving Val

instance Inherit Display where inherit = Display "inherit"
instance None    Display where none    = Display "none"
instance Other   Display where other   = Display

inline, block, listItem, runIn, inlineBlock, table, inlineTable, tableRowGroup,
  tableHeaderGroup, tableFooterGroup, tableRow, tableColumnGroup, tableColumn,
  tableCell, tableCaption, displayNone, displayInherit, inline :: Display

inline           = Display "inline"
block            = Display "block"
listItem         = Display "list-item"
runIn            = Display "runIn"
inlineBlock      = Display "inline-block"
table            = Display "table"
inlineTable      = Display "inline-table"
tableRowGroup    = Display "table-row-Group"
tableHeaderGroup = Display "table-header-group"
tableFooterGroup = Display "table-footer-group"
tableRow         = Display "table-row"
tableColumnGroup = Display "table-column-group"
tableColumn      = Display "table-column"
tableCell        = Display "table-cell"
tableCaption     = Display "table-caption"
displayNone      = Display "none"
displayInherit   = Display "inherit"

display :: Display -> Css
display = key "display"

