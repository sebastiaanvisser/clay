{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Clay.Display
(
-- * Float.

  float
, FloatStyle
, floatLeft
, floatRight
, clear
, Clear
, both
, clearLeft
, clearRight

-- * Position.

, Position
, position
, static, absolute, fixed, relative, sticky

-- * Display

, Display
, display
, inline, block, listItem, runIn, inlineBlock, table, displayTable, inlineTable, tableRowGroup
, tableHeaderGroup, tableFooterGroup, tableRow, tableColumnGroup, tableColumn
, tableCell, tableCaption, displayNone, displayInherit, flex
, inlineFlex, grid, inlineGrid

-- * Overlow

, Overflow
, scroll
, overflow, overflowX, overflowY

-- * Visibility.

, Visibility

, visibility

-- Clipping.

, Clip
, clip
, rect

-- * Opacity.

, opacity

-- * Z-index.

, zIndex

-- * Pointer-events.

, PointerEvents
, pointerEvents
, visiblePainted, visibleFill, visibleStroke, painted
, fillEvents, strokeEvents, allEvents

-- * Vertical align.

, VerticalAlign(..)
, middle, vAlignSub, vAlignSuper, textTop, textBottom, vAlignTop, vAlignBottom, vAlignBaseline

-- * Cursor

, Cursor(..)
, cursorUrl
, cursorDefault
, contextMenu, help, pointer, cursorProgress, wait
, cell, crosshair, cursorText, vText
, alias, cursorCopy, move, noDrop, notAllowed, grab, grabbing
, allScroll, colResize, rowResize, nResize, eResize, sResize, wResize
, neResize, nwResize, seResize, swResize, ewResize, nsResize, neswResize, nwseResize
, zoomIn, zoomOut
)
where

import Data.String

import Clay.Size
import Clay.Property
import Clay.Stylesheet
import Clay.Common
import Data.Text (Text)

-------------------------------------------------------------------------------

float :: FloatStyle -> Css
float = key "float"

newtype FloatStyle = FloatStyle Value
  deriving (Val,None,Inherit)

floatLeft, floatRight :: FloatStyle
floatLeft = FloatStyle "left"
floatRight = FloatStyle "right"

newtype Clear = Clear Value
  deriving (Val, Other, None, Inherit)

both :: Clear
both = Clear "both"

clearLeft :: Clear
clearLeft = Clear "left"

clearRight :: Clear
clearRight = Clear "right"

clear :: Clear -> Css
clear = key "clear"

-------------------------------------------------------------------------------

newtype Position = Position Value
  deriving (Val, Other, Inherit)

static, absolute, fixed, relative, sticky :: Position

static   = Position "static"
absolute = Position "absolute"
fixed    = Position "fixed"
relative = Position "relative"
sticky = Position $ Value (webkit <> Plain "sticky")

position :: Position -> Css
position = key "position"

-------------------------------------------------------------------------------

newtype Display = Display Value
  deriving (Val, Other, None, Inherit)

inline, block, listItem, runIn, inlineBlock, table, displayTable, inlineTable, tableRowGroup,
  tableHeaderGroup, tableFooterGroup, tableRow, tableColumnGroup, tableColumn,
  tableCell, tableCaption, displayNone, displayInherit, flex, inlineFlex, grid,
  inlineGrid :: Display

inline           = Display "inline"
block            = Display "block"
listItem         = Display "list-item"
runIn            = Display "runIn"
inlineBlock      = Display "inline-block"
displayTable     = Display "table"
{-# DEPRECATED table "Use `displayTable` instead." #-}
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
  deriving (Val, Other, Auto, Inherit, Hidden, Visible)

scroll :: Overflow
scroll = Overflow "scroll"

overflow, overflowX, overflowY :: Overflow -> Css

overflow  = key "overflow"
overflowX = key "overflow-x"
overflowY = key "overflow-y"

-------------------------------------------------------------------------------

newtype Visibility = Visibility Value
  deriving (Val, Other, Auto, Inherit, Hidden, Visible, Collapse)

visibility :: Visibility -> Css
visibility = key "visibility"

-------------------------------------------------------------------------------

newtype Clip = Clip Value
  deriving (Val, Other, Auto, Inherit)

clip :: Clip -> Css
clip = key "clip"

rect :: Size a -> Size a -> Size a -> Size a -> Clip
rect t r b l = Clip (mconcat ["rect(", value t, ",", value r, ",", value b, ",", value l, ")"])

-------------------------------------------------------------------------------

opacity :: Double -> Css
opacity = key "opacity"

zIndex :: Integer -> Css
zIndex i = key "z-index" (fromString (show i) :: Value)

-------------------------------------------------------------------------------

newtype PointerEvents = PointerEvents Value
  deriving (Val, Other, Auto, Visible, None, Inherit)

visiblePainted, visibleFill, visibleStroke, painted,
  fillEvents, strokeEvents, allEvents :: PointerEvents

visiblePainted = PointerEvents "visiblePainted"
visibleFill    = PointerEvents "visibleFill"
visibleStroke  = PointerEvents "visibleStroke"
painted        = PointerEvents "painted"
fillEvents     = PointerEvents "fill"
strokeEvents   = PointerEvents "stroke"
allEvents      = PointerEvents "all"

pointerEvents :: PointerEvents -> Css
pointerEvents = key "pointer-events"

-------------------------------------------------------------------------------

class (Val a) => VerticalAlign a where
    verticalAlign :: a -> Css
    verticalAlign = key "vertical-align"

newtype VerticalAlignValue = VerticalAlignValue Value deriving (Val, Baseline)

instance VerticalAlign VerticalAlignValue
instance VerticalAlign (Size a)

middle,vAlignSub,vAlignSuper,textTop,textBottom,vAlignTop,vAlignBottom,vAlignBaseline :: VerticalAlignValue

middle = VerticalAlignValue "middle"
vAlignSub = VerticalAlignValue "sub"
vAlignBaseline = baseline
vAlignSuper = VerticalAlignValue "super"
textTop = VerticalAlignValue "text-top"
textBottom = VerticalAlignValue "text-bottom"
vAlignTop = VerticalAlignValue "top"
vAlignBottom = VerticalAlignValue "bottom"

-------------------------------------------------------------------------------

class (Val a) => Cursor a where
    cursor :: a -> Css
    cursor = key "cursor"

newtype CursorValue a = CursorValue Value deriving (Val,Inherit,Auto,None)

instance Cursor (CursorValue a)

cursorUrl :: Text -> CursorValue Value
cursorUrl u = CursorValue $ value ("url(\"" <> u <> "\")")

-- Using the classification from https://developer.mozilla.org/en-US/docs/Web/CSS/cursor
cursorDefault
  , contextMenu, help, pointer, cursorProgress, wait
  , cell, crosshair, cursorText, vText
  , alias, cursorCopy, move, noDrop, notAllowed, grab, grabbing
  , allScroll, colResize, rowResize, nResize, eResize, sResize, wResize
  , neResize, nwResize, seResize, swResize, ewResize, nsResize, neswResize, nwseResize
  , zoomIn, zoomOut :: CursorValue Value

-- General
cursorDefault = CursorValue "default"

-- Links & status
contextMenu = CursorValue "context-menu"
help = CursorValue "help"
pointer = CursorValue "pointer"
cursorProgress = CursorValue "progress"
wait = CursorValue "wait"

-- Selection
cell = CursorValue "cell"
crosshair = CursorValue "crosshair"
cursorText = CursorValue "text"
vText = CursorValue "vertical-text"

-- Drag & drop
alias = CursorValue "alias"
cursorCopy = CursorValue "copy"
move = CursorValue "move"
noDrop = CursorValue "no-drop"
notAllowed = CursorValue "not-allowed"
grab = CursorValue "grab"
grabbing = CursorValue "grabbing"

-- Resizing & scrolling
allScroll = CursorValue "all-scroll"
colResize = CursorValue "col-resize"
rowResize = CursorValue "row-resize"
nResize = CursorValue "n-resize"
eResize = CursorValue "e-resize"
sResize = CursorValue "s-resize"
wResize = CursorValue "w-resize"

neResize = CursorValue "ne-resize"
nwResize = CursorValue "nw-resize"
seResize = CursorValue "se-resize"
swResize = CursorValue "sw-resize"
ewResize = CursorValue "ew-resize"
nsResize = CursorValue "ns-resize"
neswResize = CursorValue "nesw-resize"
nwseResize = CursorValue "nwse-resize"

-- Zooming
zoomIn = CursorValue "zoom-in"
zoomOut = CursorValue "zoom-out"
