{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Clay.Style.Properties where

import Prelude hiding (Left, Right)

import Clay.Style.Size
import Clay.Core.Property
import Clay.Core.Rule

class Inherit a where
  inherit :: a

class None a where
  none :: a

-------------------------------------------------------------------------------

data Position
  = Static
  | Absolute
  | Fixed
  | Relative
  | InheritPosition

instance Val Position where
  value Static          = "static"
  value Absolute        = "absolute"
  value Fixed           = "fixed"
  value Relative        = "relative"
  value InheritPosition = "inherit"

instance Inherit Position where
  inherit = InheritPosition

position :: Position -> Css
position = key "position"

data Display
  = Inline
  | Block
  | ListItem
  | RunIn
  | InlineBlock
  | Table
  | InlineTable
  | TableRowGroup
  | TableHeaderGroup
  | TableFooterGroup
  | TableRow
  | TableColumnGroup
  | TableColumn
  | TableCell
  | TableCaption
  | DisplayNone
  | DisplayInherit

instance Inherit Display where inherit = DisplayInherit
instance None    Display where none    = DisplayNone

instance Val Display where
  value Inline           = "inline"
  value Block            = "block"
  value ListItem         = "list-item"
  value RunIn            = "runIn"
  value InlineBlock      = "inline-block"
  value Table            = "table"
  value InlineTable      = "inline-table"
  value TableRowGroup    = "table-row-Group"
  value TableHeaderGroup = "table-header-group"
  value TableFooterGroup = "table-footer-group"
  value TableRow         = "table-row"
  value TableColumnGroup = "table-column-group"
  value TableColumn      = "table-column"
  value TableCell        = "table-cell"
  value TableCaption     = "table-caption"
  value DisplayNone      = "none"
  value DisplayInherit   = "inherit"

display :: Display -> Css
display = key "display"

size, top, left, bottom, right :: Size -> Css

size      = key "size"
top       = key "top"
left      = key "left"
bottom    = key "bottom"
right     = key "right"

width, height, minWidth, minHeight :: Size -> Css

width     = key "width"
height    = key "height"
minWidth  = key "min-width"
minHeight = key "min-height"

-------------------------------------------------------------------------------

padding, margin :: Size -> Size -> Size -> Size -> Css

padding = key4 "padding"
margin  = key4 "margin"

paddingTop, paddingLeft, paddingRight, paddingBottom,
  marginTop, marginLeft, marginRight, marginBottom :: Size -> Css

paddingTop    = key "paddingTop"
paddingLeft   = key "paddingLeft"
paddingRight  = key "paddingRight"
paddingBottom = key "paddingBottom"
marginTop     = key "marginTop"
marginLeft    = key "marginLeft"
marginRight   = key "marginRight"
marginBottom  = key "marginBottom"

sym4 :: (Size -> Size -> Size -> Size -> Css) -> Size -> Css
sym4 k a = k a a a a

sym3 :: (Size -> Size -> Size -> Size -> Css) -> Size -> Size -> Size -> Css
sym3 k tb l r = k tb l tb r

sym2 :: (Size -> Size -> Size -> Size -> Css) -> Size -> Size -> Css
sym2 k tb lr = k tb lr tb lr

