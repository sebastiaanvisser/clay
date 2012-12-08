{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Clay.Display where

import Data.Monoid

import Clay.Size
import Clay.Property
import Clay.Rule
import Clay.Common

-------------------------------------------------------------------------------

newtype Position = Position Value
  deriving Val

instance Other   Position where other   = Position
instance Inherit Position where inherit = Position "inherit"

static, absolute, fixed, relative :: Position

static   = Position "static"
absolute = Position "absolute"
fixed    = Position "fixed"
relative = Position "relative"

position :: Position -> Css
position = key "position"

-------------------------------------------------------------------------------

newtype Display = Display Value
  deriving (Val, None, Inherit, Other)

inline, block, listItem, runIn, inlineBlock, table, inlineTable, tableRowGroup,
  tableHeaderGroup, tableFooterGroup, tableRow, tableColumnGroup, tableColumn,
  tableCell, tableCaption, displayNone, displayInherit, inline, flex,
  inlineFlex, grid, inlineGrid :: Display

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
flex             = Display "flex"
inlineFlex       = Display "inline-flex"
grid             = Display "grid"
inlineGrid       = Display "inline-grid"

display :: Display -> Css
display = key "display"

-------------------------------------------------------------------------------

newtype Overflow = Overflow Value
  deriving (Val, Auto, Inherit, Hidden, Visible)

scroll :: Overflow
scroll = Overflow "scroll"

overflow :: Overflow -> Css
overflow = key "overflow"

overflowX :: Overflow -> Css
overflowX = key "overflow-y"

overflowY :: Overflow -> Css
overflowY = key "overflow-x"

-------------------------------------------------------------------------------

newtype Visibility = Visibility Value
  deriving (Val, Auto, Inherit, Hidden, Visible)

collapse :: Visibility
collapse = Visibility "collapse"

visibility :: Visibility -> Css
visibility = key "overflow"

-------------------------------------------------------------------------------

newtype Clip = Clip Value
  deriving (Val, Auto, Inherit)

clip :: Clip -> Css
clip = key "clip"

rect :: Size a -> Size a -> Size a -> Size a -> Clip
rect t r b l = Clip (mconcat ["rect(", value t, ",", value r, ",", value b, ",", value l, ")"])

