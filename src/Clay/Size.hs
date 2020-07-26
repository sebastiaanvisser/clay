{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  , FlexibleInstances
  , ExistentialQuantification
  , StandaloneDeriving
  , TypeFamilies
  , EmptyDataDecls
  #-}
module Clay.Size
(

-- * Size type.
  Size
, LengthUnit
, Percentage
, AnyUnit
, upcast
, nil
, unitless

-- * Size constructors.

, cm
, mm
, inches
, px
, pt
, pc
, em
, ex
, pct
, rem
, vw
, vh
, vmin
, vmax
, fr
, maxContent
, minContent
, fitContent
, minmax

-- * Calculation operators for calc

, (@+@)
, (@-@)
, (@*)
, (*@)
, (@/)

-- * Shorthands for properties that can be applied separately to each box side.

, sym
, sym2
, sym3

-- * Angle type.

, Angle
, Deg
, Rad
, Grad
, Turn

-- * Constructing angles.

, deg
, rad
, grad
, turn

)
where

import Data.Monoid
import Prelude hiding (rem)
import Data.Text (Text)
import Data.Coerce (coerce)

import Clay.Common
import Clay.Property
import Clay.Stylesheet

-------------------------------------------------------------------------------

-- | Sizes can be given using a length unit (e.g. em, px).
data LengthUnit

-- | Sizes can be given in percentages.
data Percentage

-- | When combining percentages with units using calc, we get a combination
-- | Any unit or combination of units
data AnyUnit

-- | Upcast a size unit
upcast :: Size a -> Size AnyUnit
upcast = coerce

data Size a =
  SimpleSize Text |
  forall b c. SumSize (Size b) (Size c) |
  forall b c. DiffSize (Size b) (Size c) |
  forall b c. MinMaxSize (Size b) (Size c) |
  forall b. FitContentSize (Size b) |
  MultSize Double (Size a) |
  DivSize Double (Size a) |
  OtherSize Value

deriving instance Show (Size a)

sizeToText :: Size a -> Text
sizeToText (SimpleSize txt) = txt
sizeToText (SumSize a b) = mconcat ["(", sizeToText a, " + ", sizeToText b, ")"]
sizeToText (DiffSize a b) = mconcat ["(", sizeToText a, " - ", sizeToText b, ")"]
sizeToText (MultSize a b) = mconcat ["(", cssDoubleText a, " * ", sizeToText b, ")"]
sizeToText (DivSize a b) = mconcat ["(", sizeToText b, " / ", cssDoubleText a, ")"]
sizeToText (MinMaxSize a b) = mconcat ["minmax(", sizeToText a, ",", sizeToText b, ")"]
sizeToText (FitContentSize a) = mconcat ["fit-content(", sizeToText a, ")"]
sizeToText (OtherSize a) = plain $ unValue a

instance Val (Size a) where
  value (SimpleSize a) = value a
  value (OtherSize a) = a
  value s@(MinMaxSize _ _) = Value $ Plain $ sizeToText s
  value s@(FitContentSize _) = Value $ Plain $ sizeToText s
  value s = Value $ Plain ("calc" <> sizeToText s)

-- Keywords
instance Auto       (Size a) where auto       = OtherSize auto
instance Inherit    (Size a) where inherit    = OtherSize inherit
instance Initial    (Size a) where initial    = OtherSize initial
instance Unset      (Size a) where unset      = OtherSize inherit
instance None       (Size a) where none       = OtherSize none
instance MinContent (Size a) where minContent = OtherSize minContent
instance MaxContent (Size a) where maxContent = OtherSize maxContent

instance Other (Size a) where other a = OtherSize a

-- | Zero size.
nil :: Size a
nil = SimpleSize "0"

-- | Unitless size (as recommended for line-height).
unitless :: Double -> Size a
unitless i = SimpleSize ((plain . unValue . value) i)

cm, mm, inches, px, pt, pc :: Double -> Size LengthUnit

-- | Size in centimeters.
cm i = SimpleSize (cssDoubleText i <> "cm")

-- | Size in millimeters.
mm i = SimpleSize (cssDoubleText i <> "mm")

-- | Size in inches (1in = 2.54 cm).
inches i = SimpleSize (cssDoubleText i <> "in")

-- | Size in pixels.
px i = SimpleSize (cssDoubleText i <> "px")

-- | Size in points (1pt = 1/72 of 1in).
pt i = SimpleSize (cssDoubleText i <> "pt")

-- | Size in picas (1pc = 12pt).
pc i = SimpleSize (cssDoubleText i <> "pc")

em, ex, rem, vw, vh, vmin, vmax, fr :: Double -> Size LengthUnit

-- | Size in em's (computed cssDoubleText of the font-size).
em i = SimpleSize (cssDoubleText i <> "em")

-- | SimpleSize in ex'es (x-height of the first avaliable font).
ex i = SimpleSize (cssDoubleText i <> "ex")

-- | SimpleSize in rem's (em's, but always relative to the root element).
rem i = SimpleSize (cssDoubleText i <> "rem")

-- | SimpleSize in vw's (1vw = 1% of viewport width).
vw i = SimpleSize (cssDoubleText i <> "vw")

-- | SimpleSize in vh's (1vh = 1% of viewport height).
vh i = SimpleSize (cssDoubleText i <> "vh")

-- | SimpleSize in vmin's (the smaller of vw or vh).
vmin i = SimpleSize (cssDoubleText i <> "vmin")

-- | SimpleSize in vmax's (the larger of vw or vh).
vmax i = SimpleSize (cssDoubleText i <> "vmax")

-- | 'SimpleSize' in fr's (a fractional unit and 1fr is for 1 part of the available space in grid areas).
fr i = SimpleSize (cssDoubleText i <> "fr")

-- | The larger of the intrinsic minimum width or the smaller of the intrinsic preferred width and the available width.
fitContent :: Size a -> Size a
fitContent = FitContentSize

-- | A mixed size range; only valid within grid-template-rows, grid-template-columns, grid-auto-rows, or grid-auto-columns.
minmax :: Size a -> Size b -> Size AnyUnit
minmax = MinMaxSize

-- | SimpleSize in percents.
pct :: Double -> Size Percentage
pct i = SimpleSize (cssDoubleText i <> "%")

instance Num (Size LengthUnit) where
  fromInteger = px . fromInteger
  (+)    = error   "plus not implemented for Size"
  (*)    = error  "times not implemented for Size"
  abs    = error    "abs not implemented for Size"
  signum = error "signum not implemented for Size"
  negate = error "negate not implemented for Size"

instance Fractional (Size LengthUnit) where
  fromRational = px . fromRational
  recip  = error  "recip not implemented for Size"

instance Num (Size Percentage) where
  fromInteger = pct . fromInteger
  (+)    = error   "plus not implemented for Size"
  (*)    = error  "times not implemented for Size"
  abs    = error    "abs not implemented for Size"
  signum = error "signum not implemented for Size"
  negate = error "negate not implemented for Size"

instance Fractional (Size Percentage) where
  fromRational = pct . fromRational
  recip  = error  "recip not implemented for Size"

-- | Type family to define what is the result of a calc operation

type family SizeCombination sa sb where
  SizeCombination Percentage Percentage = Percentage
  SizeCombination LengthUnit LengthUnit = LengthUnit
  SizeCombination a b = AnyUnit

-- | Plus operator to combine sizes into calc function
infixl 6 @+@
(@+@) :: Size a -> Size b -> Size (SizeCombination a b)
a @+@ b = SumSize a b

-- | Minus operator to combine sizes into calc function
infixl 6 @-@
(@-@) :: Size a -> Size b -> Size (SizeCombination a b)
a @-@ b = DiffSize a b

-- | Times operator to combine sizes into calc function
infixl 7 *@
(*@) :: Double -> Size a -> Size a
a *@ b = MultSize a b

-- | Reversed times operator to combine sizes into calc function
infixl 7 @*
(@*) :: Size a -> Double -> Size a
a @* b = MultSize b a

-- | Division operator to combine sizes into calc function
infixl 7 @/
(@/) :: Size a -> Double -> Size a
a @/ b = DivSize b a

-------------------------------------------------------------------------------

sym :: (a -> a -> a -> a -> Css) -> a -> Css
sym k a = k a a a a

sym3 :: (tb -> l -> tb -> r -> Css) -> tb -> l -> r -> Css
sym3 k tb l r = k tb l tb r

sym2 :: (tb -> lr -> tb -> lr -> Css) -> tb -> lr -> Css
sym2 k tb lr = k tb lr tb lr

-------------------------------------------------------------------------------

data Deg
data Rad
data Grad
data Turn

newtype Angle a = Angle Value
  deriving (Val, Auto, Inherit, Other)

-- | Angle in degrees.
deg :: Double -> Angle Deg
deg i = Angle (value i <> "deg")

-- | Angle in radians.
rad :: Double -> Angle Rad
rad i = Angle (value i <> "rad")

-- | Angle in gradians (also knows as gons or grades).
grad :: Double -> Angle Grad
grad i = Angle (value i <> "grad")

-- | Angle in turns.
turn :: Double -> Angle Turn
turn i = Angle (value i <> "turn")

instance Num (Angle Deg) where
  fromInteger = deg . fromInteger
  (+)    = error   "plus not implemented for Angle"
  (*)    = error  "times not implemented for Angle"
  abs    = error    "abs not implemented for Angle"
  signum = error "signum not implemented for Angle"
  negate = error "negate not implemented for Angle"

instance Fractional (Angle Deg) where
  fromRational = deg . fromRational
  recip  = error  "recip not implemented for Angle"

instance Num (Angle Rad) where
  fromInteger = rad . fromInteger
  (+)    = error   "plus not implemented for Angle"
  (*)    = error  "times not implemented for Angle"
  abs    = error    "abs not implemented for Angle"
  signum = error "signum not implemented for Angle"
  negate = error "negate not implemented for Angle"

instance Fractional (Angle Rad) where
  fromRational = rad . fromRational
  recip  = error  "recip not implemented for Angle"

instance Num (Angle Grad) where
  fromInteger = grad . fromInteger
  (+)    = error   "plus not implemented for Angle"
  (*)    = error  "times not implemented for Angle"
  abs    = error    "abs not implemented for Angle"
  signum = error "signum not implemented for Angle"
  negate = error "negate not implemented for Angle"

instance Fractional (Angle Grad) where
  fromRational = grad . fromRational
  recip  = error  "recip not implemented for Angle"

instance Num (Angle Turn) where
  fromInteger = turn . fromInteger
  (+)    = error   "plus not implemented for Angle"
  (*)    = error  "times not implemented for Angle"
  abs    = error    "abs not implemented for Angle"
  signum = error "signum not implemented for Angle"
  negate = error "negate not implemented for Angle"

instance Fractional (Angle Turn) where
  fromRational = turn . fromRational
  recip  = error  "recip not implemented for Angle"
