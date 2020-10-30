{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Clay.Flexbox where

import Clay.Common     (Auto, Baseline, Center, Inherit, Other, Row, Column)
import Clay.Property
import Clay.Size       (Size)
import Clay.Stylesheet
import Data.String     (fromString)

-- | CSS Flexible Box Layout
-- http://dev.w3.org/csswg/css-flexbox-1

class FlexEnd      a where flexEnd      :: a
class FlexStart    a where flexStart    :: a
class SpaceAround  a where spaceAround  :: a
class SpaceBetween a where spaceBetween :: a
class Stretch      a where stretch      :: a

instance FlexEnd Value      where flexEnd      = "flex-end"
instance FlexStart Value    where flexStart    = "flex-start"
instance SpaceAround Value  where spaceAround  = "space-around"
instance SpaceBetween Value where spaceBetween = "space-between"
instance Stretch Value      where stretch      = "stretch"

-------------------------------------------------------------------------------

newtype AlignContentValue = AlignContentValue Value
  deriving (Val, Other, Inherit, FlexStart, FlexEnd
          , Center, SpaceBetween, SpaceAround, Stretch)

alignContent :: AlignContentValue -> Css
alignContent = key "align-content"

-------------------------------------------------------------------------------

newtype AlignItemsValue = AlignItemValue Value
  deriving (Val, Other, Inherit, Baseline
          , Center, FlexEnd, FlexStart, Stretch)

alignItems :: AlignItemsValue -> Css
alignItems = key "align-items"

-------------------------------------------------------------------------------

newtype AlignSelfValue = AlignSelfValue Value
  deriving (Val, Other, Inherit, Auto, Baseline
          , Center, FlexEnd, FlexStart, Stretch)

alignSelf :: AlignSelfValue -> Css
alignSelf = key "align-self"

-------------------------------------------------------------------------------

flex :: Int -> Int -> Size b -> Css
flex g s b = key "flex" (gs ! ss ! value b)
  where gs = fromString (show g) :: Value
        ss = fromString (show s) :: Value

-------------------------------------------------------------------------------

flexBasis :: Size a -> Css
flexBasis = key "flex-basis"

-------------------------------------------------------------------------------

newtype FlexDirection = FlexDirection Value
  deriving (Val, Other, Row, Column)

rowReverse, columnReverse :: FlexDirection
rowReverse    = FlexDirection "row-reverse"
columnReverse = FlexDirection "column-reverse"

flexDirection :: FlexDirection -> Css
flexDirection = key "flex-direction"

-------------------------------------------------------------------------------

flexFlow :: FlexDirection -> FlexWrap -> Css
flexFlow d w = key "flex-flow" (d ! w)

-------------------------------------------------------------------------------

flexGrow :: Int -> Css
flexGrow i = key "flex-grow" (fromString (show i) :: Value)

flexShrink :: Int  -> Css
flexShrink i = key "flex-shrink" (fromString (show i) :: Value)

-------------------------------------------------------------------------------

newtype FlexWrap = FlexWrap Value
  deriving (Val, Other)

nowrap, wrap, wrapReverse :: FlexWrap

nowrap = FlexWrap "nowrap"
wrap = FlexWrap "wrap"
wrapReverse = FlexWrap "wrap-reverse"

flexWrap :: FlexWrap -> Css
flexWrap = key "flex-wrap"

-------------------------------------------------------------------------------

newtype JustifyContentValue = JustifyContentValue Value
  deriving (Val, Other, Inherit, Center, FlexEnd
          , FlexStart, SpaceAround, SpaceBetween)

justifyContent :: JustifyContentValue -> Css
justifyContent = key "justify-content"

-------------------------------------------------------------------------------

order :: Int -> Css
order i = key "order" (fromString (show i) :: Value)
