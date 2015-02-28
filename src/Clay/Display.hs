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
, static, absolute, fixed, relative

-- * Display

, Display
, display
, inline, block, listItem, runIn, inlineBlock, table, inlineTable, tableRowGroup
, tableHeaderGroup, tableFooterGroup, tableRow, tableColumnGroup, tableColumn
, tableCell, tableCaption, displayNone, displayInherit, flex
, inlineFlex, grid, inlineGrid

-- * Overlow

, Overflow
, scroll
, overflow, overflowX, overflowY

-- * Visibility.

, Visibility
, collapse, separate

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
, middle, vAlignSub, vAlignSuper, textTop, textBottom, vAlignTop, vAlignBottom

-- * Cursor

, Cursor(..)
, crosshair, cursorDefault, pointer, move, eResize, neResize, nwResize, nResize, seResize, swResize, sResize, wResize, cursorText, wait, cursorProgress, help, cursorUrl

)
where

import Data.Monoid
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

static, absolute, fixed, relative :: Position

static   = Position "static"
absolute = Position "absolute"
fixed    = Position "fixed"
relative = Position "relative"

position :: Position -> Css
position = key "position"

-------------------------------------------------------------------------------

newtype Display = Display Value
  deriving (Val, Other, None, Inherit)

inline, block, listItem, runIn, inlineBlock, table, inlineTable, tableRowGroup,
  tableHeaderGroup, tableFooterGroup, tableRow, tableColumnGroup, tableColumn,
  tableCell, tableCaption, displayNone, displayInherit, flex, inlineFlex, grid,
  inlineGrid :: Display

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
  deriving (Val, Other, Auto, Inherit, Hidden, Visible)

scroll :: Overflow
scroll = Overflow "scroll"

overflow, overflowX, overflowY :: Overflow -> Css

overflow  = key "overflow"
overflowX = key "overflow-x"
overflowY = key "overflow-y"

-------------------------------------------------------------------------------

newtype Visibility = Visibility Value
  deriving (Val, Other, Auto, Inherit, Hidden, Visible)

separate, collapse :: Visibility

collapse = Visibility "collapse"
separate = Visibility "separate"

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

newtype VerticalAlignValue a = VerticalAlignValue Value deriving (Val, Baseline)

instance VerticalAlign (VerticalAlignValue a)
instance VerticalAlign (Size a)

middle,vAlignSub,vAlignSuper,textTop,textBottom,vAlignTop,vAlignBottom :: VerticalAlignValue Value

middle = VerticalAlignValue "middle"
vAlignSub = VerticalAlignValue "sub"
vAlignSuper = VerticalAlignValue "super"
textTop = VerticalAlignValue "text-top"
textBottom = VerticalAlignValue "text-bottom"
vAlignTop = VerticalAlignValue "top"
vAlignBottom = VerticalAlignValue "bottom"

-------------------------------------------------------------------------------               

class (Val a) => Cursor a where
    cursor :: a -> Css
    cursor = key "cursor"

newtype CursorValue a = CursorValue Value deriving (Val,Inherit,Auto)

instance Cursor (CursorValue a)

crosshair,cursorDefault,pointer,move,eResize,neResize,nwResize,nResize,seResize,swResize,sResize,wResize,cursorText,wait,cursorProgress,help :: CursorValue Value
                                                                                                                                          
crosshair = CursorValue "crosshair"
cursorDefault = CursorValue "cursorDefault"
pointer = CursorValue "pointer"
move = CursorValue "move"
eResize = CursorValue "e-resize"
neResize = CursorValue "ne-resize"
nwResize = CursorValue "nw-resize"
nResize = CursorValue "n-resize"
seResize = CursorValue "se-resize"
swResize = CursorValue "sw-resize"
sResize = CursorValue "sResize"
wResize = CursorValue "sResize"
cursorText = CursorValue "text"
wait = CursorValue "wait"
cursorProgress = CursorValue "progress"
help = CursorValue "help"

cursorUrl :: Text -> CursorValue Value
cursorUrl u = CursorValue $ value ("url(\"" <> u <> "\")")
