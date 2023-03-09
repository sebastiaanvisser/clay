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
, ch
, pct
, rem
, lh
, rlh
, vw
, vh
, vmin
, vmax
, vb
, vi
, svw
, svh
, lvw
, lvh
, dvw
, dvh
, fr
, maxContent
, minContent
, available
, fitContent

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

import Clay.Common
import Clay.Property
import Clay.Stylesheet

-------------------------------------------------------------------------------

-- | Sizes can be given using a length unit (e.g. em, px).
data LengthUnit

-- | Sizes can be given in percentages.
data Percentage

-- | When combining percentages with units using calc, we get a combination
data Combination

data Size a =
  SimpleSize Text |
  forall b c. SumSize (Size b) (Size c) |
  forall b c. DiffSize (Size b) (Size c) |
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
sizeToText (OtherSize a) = plain $ unValue a

instance Val (Size a) where
  value (SimpleSize a) = value a
  value (OtherSize a) = a
  value s = Value $ browsers <> Plain ("calc" <> sizeToText s)

instance Auto (Size a) where auto = OtherSize Clay.Common.autoValue
instance Normal (Size a) where normal = OtherSize Clay.Common.normalValue
instance Inherit (Size a) where inherit = OtherSize Clay.Common.inheritValue
instance None (Size a) where none = OtherSize Clay.Common.noneValue
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

em, ex, ch, rem, lh, rlh, vw, vh, vmin, vmax, vb, vi, svw, svh, lvw, lvh, dvw, dvh, fr :: Double -> Size LengthUnit

-- | Size in em's (computed cssDoubleText of the font-size).
em i = SimpleSize (cssDoubleText i <> "em")

-- | SimpleSize in ex'es (x-height of the first avaliable font).
ex i = SimpleSize (cssDoubleText i <> "ex")

-- | SimpleSize in ch's (The width of the glyph "0" of the element's font).
ch i = SimpleSize (cssDoubleText i <> "ch")

-- | SimpleSize in rem's (em's, but always relative to the root element).
rem i = SimpleSize (cssDoubleText i <> "rem")

-- | SimpleSize in lh's (Line height of the element).
lh i = SimpleSize (cssDoubleText i <> "lh")

-- | SimpleSize in rlh's (lh's, but always relative to the root element).
rlh i = SimpleSize (cssDoubleText i <> "rlh")

-- | SimpleSize in vw's (1vw = 1% of viewport width).
vw i = SimpleSize (cssDoubleText i <> "vw")

-- | SimpleSize in vh's (1vh = 1% of viewport height).
vh i = SimpleSize (cssDoubleText i <> "vh")

-- | SimpleSize in vmin's (the smaller of vw or vh).
vmin i = SimpleSize (cssDoubleText i <> "vmin")

-- | SimpleSize in vmax's (the larger of vw or vh).
vmax i = SimpleSize (cssDoubleText i <> "vmax")

-- | SimpleSize in vb's (1vb = 1% of the parent's size in the direction of the root element's block axis).
vb i = SimpleSize (cssDoubleText i <> "vb")

-- | SimpleSize in vi's (1vi = 1% of the parent's size in the direction of the root element's inline axis).
vi i = SimpleSize (cssDoubleText i <> "vi")

-- | SimpleSize in svw's (1svw = 1% of the small viewport's width).
svw i = SimpleSize (cssDoubleText i <> "svw")

-- | SimpleSize in svh's (1svh = 1% of the small viewport's height).
svh i = SimpleSize (cssDoubleText i <> "svh")

-- | SimpleSize in lvw's (1lvw = 1% of the large viewport's width).
lvw i = SimpleSize (cssDoubleText i <> "lvw")

-- | SimpleSize in lvh's (1lvh = 1% of the large viewport's height).
lvh i = SimpleSize (cssDoubleText i <> "lvh")

-- | SimpleSize in dvw's (1dvw = 1% of the dynamic viewport's width).
dvw i = SimpleSize (cssDoubleText i <> "dvw")

-- | SimpleSize in dvh's (1dvh = 1% of the dynamic viewport's height).
dvh i = SimpleSize (cssDoubleText i <> "dvh")

-- | 'SimpleSize' in fr's (a fractional unit and 1fr is for 1 part of the available space in grid areas).
fr i = SimpleSize (cssDoubleText i <> "fr")

-- | SimpleSize for the intrinsic preferred width.
maxContent :: Size LengthUnit
maxContent = SimpleSize "max-content"

-- | SimpleSize for the intrinsic minimum width.
minContent :: Size LengthUnit
minContent = SimpleSize "min-content"

-- | SimpleSize for the containing block width minus horizontal margin, border, and padding.
available :: Size LengthUnit
available = SimpleSize "available"

-- | The larger of the intrinsic minimum width or the smaller of the intrinsic preferred width and the available width.
fitContent :: Size LengthUnit
fitContent = SimpleSize "fit-content"

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
  SizeCombination a b = Combination

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
