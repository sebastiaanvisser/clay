{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | Partial implementation of <https://alligator.io/css/css-grid-layout-grid-areas grid area CSS API>.
module Clay.Grid
(
    -- * Grid
    --
    -- $gridIntro
    gridGap
  , gridTemplateColumns

    -- * Size and location
    --
    -- $sizeAndLocationIntro

    -- ** Data types and type classes
  , GridLine (..)
  , ToGridLine
  , toGridLine
  , GridLines2 (..)
  , ToGridLines2
  , toGridLines2
  , GridLines4 (..)
  , ToGridLines4
  , toGridLines4
  , OneGridLine
  , TwoGridLines
  , ThreeGridLines
  , FourGridLines
  , CustomIdentGrid
  , customIdentToText
  , partialMkCustomIdentGrid
  , ToSpan

    -- ** Style properties
    --
    -- $invalidValues
  , gridArea
  , gridColumn
  , gridColumnStart
  , gridColumnEnd
  , gridRow
  , gridRowStart
  , gridRowEnd

    -- ** Keywords
  , (//)
  , span_
)
where

import qualified Clay.Common as Com
import           Clay.Property (Val, noCommas, value)
import           Clay.Size (Size)
import           Clay.Stylesheet (Css, key)
import           Data.Char (isNumber)
import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude

-- $gridIntro
-- @grid-gap@ and @grid-template@ CSS properties.
--
-- === Example
-- For the below CSS code:
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
-- The corresponding clay code is:
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

-- | Property sets the gaps (gutters) between rows and columns.
gridGap :: Size a -> Css
gridGap = key "grid-gap"

-- | Property defines the line names and track sizing functions of the grid columns.
gridTemplateColumns :: [Size a] -> Css
gridTemplateColumns = key "grid-template-columns" . noCommas

-- $sizeAndLocationIntro
--
-- == CSS documentation
-- The below functions are based on
-- [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/CSS)
-- CSS documentation.
--
-- === __Naming note__
-- In this documentation, as the functions are polymorphic we sometimes
-- refer to the CSS types as used in the
-- [mdm web docs_](https://developer.mozilla.org/en-US/docs/Web/CSS)
-- rather than the Haskell types.
-- For example, @grid-line@ is used instead of 'GridLine' as a the argument
-- might be provided as a 'GridLine' but also as an 'Integer', 'String', etc.
--
-- #pragma#
--
-- == Pragma
-- If you want to avoid specifying the types of the arguments, enable
-- the @ExtendedDefaultRules@ GHC language pragma as well as the
-- @-Wno-type-defaults@ GHC option to avoid compilation warnings.
--
-- @
-- {-# LANGUAGE ExtendedDefaultRules #-}
-- {-# OPTIONS_GHC -Wno-type-defaults #-}
-- @
--
-- === __Examples__
-- With the above enabled, you can write:
--
-- >> gridRowStart 2
--
-- >> gridRowStart "somegridarea"
--
-- If you do not enable those, then you must write:
--
-- >> gridRowStart (2 :: Integer)
--
-- >> gridRowStart ("somegridarea" :: String)
--
-- === __Note__
-- If you decide to enable the above, it is advisable to have your Clay
-- CSS code in its own module, so the behaviour of the rest of your code
-- is not affected.
--
-- == Examples
-- Examples are provided through the documentation for the various functions.
-- Further examples can be found in the source code of the test suite
-- in the GridSpec.hs module.

-- | A @grid-line@ value.
--
-- A @grid-line@ value specifies a size and location in a grid.
--
-- __NOTE:__ although you can use the below constructors, it's also possible
-- to use a closer CSS syntax taking advantage of the 'ToGridLine' instances.
data GridLine

  -- | 'Integer' value.
  --
  -- __NOTE:__ 'Integer' value of 0 is invalid.
  = Coordinate Integer

  -- | @custom-ident@ with an optional 'Integer' value.
  --
  -- __NOTE:__ 'Integer' value of 0 is invalid.
  | GridLineCustomIdent CustomIdentGrid (Maybe Integer)

  -- | @span@ CSS keyword with an optional @custom-ident@ and/or 'Integer' value.
  --
  -- __NOTE:__ negative 'Integer' or 0 are invalid.
  | Span (Maybe CustomIdentGrid) (Maybe Integer)

  -- | @auto@ CSS keyword.
  | Auto

  -- | @inherit@ CSS keyword.
  | Inherit

  -- | @initial@ CSS keyword.
  | Initial

  -- | @unset@ CSS keyword.
  | Unset
  deriving (Eq, Show)

class ToGridLine a where
  -- | Convert the provided type to a 'GridLine'.
  toGridLine :: a -> GridLine

instance ToGridLine GridLine where
  toGridLine = id

instance ToGridLine Integer where
  -- | __NOTE:__ 'Integer' value of 0 is invalid.
  toGridLine = Coordinate

-- | @custom-ident@ value.
instance ToGridLine CustomIdentGrid where
  toGridLine x = GridLineCustomIdent x Nothing

-- | @custom-ident@ value.
instance ToGridLine String where
  toGridLine = toGridLine . CustomIdentGrid . T.pack

-- | Both @custom-ident@ and `Integer` values, provided as a pair.
--
-- __NOTE:__ 'Integer' value of 0 is invalid.
instance ToGridLine (CustomIdentGrid, Integer) where
  toGridLine (x, y) = GridLineCustomIdent x (Just y)

-- | Both @custom-ident@ and `Integer` values, provided as a pair.
--
-- __NOTE:__ 'Integer' value of 0 is invalid.
instance ToGridLine (String, Integer) where
  toGridLine (x, y) = toGridLine (CustomIdentGrid $ T.pack x, y)

-- | One or two @grid-line@ values.
--
-- __NOTE:__ although you can use the below constructors, it's also possible
-- to use a closer CSS syntax using the 'ToGridLines2' instances.
data GridLines2
    -- | One @grid-line@ value.
  = One2 OneGridLine
    -- | Two @grid-line@ values.
  | Two2 TwoGridLines

class ToGridLines2 a where

  -- | Convert the provided type to 'GridLines2' (one or two @grid-line@ values).
  toGridLines2 :: a -> GridLines2

-- | One @grid-line@ value.
instance ToGridLines2 GridLine where
  toGridLines2 = One2 . OneGridLine

-- | One @grid-line@ value.
instance ToGridLines2 OneGridLine where
  toGridLines2 = One2

-- | Two @grid-line@ values.
instance ToGridLines2 TwoGridLines where
  toGridLines2 = Two2

-- | One or two @grid-line@ values.
instance ToGridLines2 GridLines2 where
  toGridLines2 = id

-- | One 'Integer' value.
--
-- __NOTE:__ 'Integer' value of 0 is invalid.
instance ToGridLines2 Integer where
  toGridLines2 = toGridLines2 . toGridLine

-- | One @custom-ident@ value.
instance ToGridLines2 CustomIdentGrid where
  toGridLines2 = toGridLines2 . toGridLine

-- | One @custom-ident@ value.
instance ToGridLines2 String where
  toGridLines2 = toGridLines2 . toGridLine

-- | One time both a @custom-ident@ and 'Integer' values, provided as a pair.
--
-- __NOTE:__ 'Integer' value of 0 is invalid.
instance ToGridLines2 (CustomIdentGrid, Integer) where
  toGridLines2 = toGridLines2 . toGridLine

-- | One time both a @custom-ident@ and 'Integer' values, provided as a pair.
--
-- __NOTE:__ 'Integer' value of 0 is invalid.
instance ToGridLines2 (String, Integer) where
  toGridLines2 = toGridLines2 . toGridLine

-- | One, two, three or four @grid-line@ values.
--
-- __NOTE:__ although you can use the below constructors, it's also possible
-- to use a closer CSS syntax using the 'ToGridLines4' instances.
data GridLines4

    -- | One @grid-line@ value.
  = One4 OneGridLine

    -- | Two @grid-line@ values.
  | Two4 TwoGridLines

    -- | Three @grid-line@ values.
  | Three4 ThreeGridLines

    -- | Four @grid-line@ values.
  | Four4 FourGridLines

class ToGridLines4 a where
  -- | Convert the provided type to 'GridLines4'
  -- (one, two, three or four @grid-line@ values).
  toGridLines4 :: a -> GridLines4

-- | One @grid-line@ value.
instance ToGridLines4 GridLine where
  toGridLines4 = One4 . OneGridLine

-- | One @grid-line@ value.
instance ToGridLines4 OneGridLine where
  toGridLines4 = One4

-- | Two @grid-line@ values.
instance ToGridLines4 TwoGridLines where
  toGridLines4 = Two4

-- | Three @grid-line@ values.
instance ToGridLines4 ThreeGridLines where
  toGridLines4 = Three4

-- | Four @grid-line@ values.
instance ToGridLines4 FourGridLines where
  toGridLines4 = Four4

-- | One, two, three or four @grid-line@ values.
instance ToGridLines4 GridLines4 where
  toGridLines4 = id

-- | One 'Integer' value.
--
-- __NOTE:__ 'Integer' value of 0 is invalid.
instance ToGridLines4 Integer where
  toGridLines4 = toGridLines4 . toGridLine

-- | One @custom-ident@ value.
instance ToGridLines4 CustomIdentGrid where
  toGridLines4 = toGridLines4 . toGridLine

-- | One @custom-ident@ value.
instance ToGridLines4 String where
  toGridLines4 = toGridLines4 . toGridLine

-- | One time both a @custom-ident@ and 'Integer' values, provided as a pair.
--
-- __NOTE:__ 'Integer' value of 0 is invalid.
instance ToGridLines4 (CustomIdentGrid, Integer) where
  toGridLines4 = toGridLines4 . toGridLine

-- | One time both a @custom-ident@ and 'Integer' values, provided as a pair.
--
-- __NOTE:__ 'Integer' value of 0 is invalid.
instance ToGridLines4 (String, Integer) where
  toGridLines4 = toGridLines4 . toGridLine

-- | One 'GridLine' value.
newtype OneGridLine = OneGridLine GridLine

-- | Two 'GridLine' values.
data TwoGridLines = TwoGridLines GridLine GridLine

-- | Three 'GridLine' values.
data ThreeGridLines = ThreeGridLines GridLine GridLine GridLine

-- | Four 'GridLine' values.
data FourGridLines = FourGridLines GridLine GridLine GridLine GridLine

-- | CSS @custom-ident@.
--
-- The data constructor is not exported. Use the 'partialMkCustomIdentGrid'
-- smart constructor to create a 'CustomIdentGrid'.
--
-- === __Note__
-- In CSS, some values for @custom-ident@ are invalid depending on the CSS
-- property the @custom-ident@ is used with.
-- Consequently, the @custom-ident@ is only for CSS grid.
newtype CustomIdentGrid = CustomIdentGrid Text deriving (Eq, Ord, Read, Show)

-- | Convert a 'CustomIdentGrid' to 'Text'.
--
-- === __Note__
-- The function is defined on its own as the newtype constructor
-- of 'CustomIdentGrid'is not exported.
customIdentToText :: CustomIdentGrid -> Text
customIdentToText (CustomIdentGrid x) = x

-- | Create a 'CustomIdentGrid'.
--
-- __WARNING__: this function is partial. An error will be raised if:
--
-- * "span" is provided as a value (this is a reserved keyword in this context)
-- * a number is provided as first character
-- * __If__ a hyphen (-) is provided as first character:
--
--     ** a number is provided as second character
--     ** a hyphen (-) is provided as second character.
--
-- === __Note__
-- The above is a partial implementation of the CSS custom-ident naming rules.
-- If you stick to the following charset @[a-zA-z0-9-_]@ the checks will be effective
-- and your CSS custom-ident will be a valid one.
-- On the other hand, the following will not be checked:
--
-- * character escaping (for example @\?@) or unicode
-- * characters provided as hexadecimal number (for example @\0x03BB@)
-- * characters outside of the above charset.
--
-- More information regarding this topic can be found on
-- [mdm web docs_](https://developer.mozilla.org/en-US/docs/Web/CSS/custom-ident)
partialMkCustomIdentGrid :: Text -> CustomIdentGrid
partialMkCustomIdentGrid "span" = error "Custom-ident for a grid property cannot be named span"
partialMkCustomIdentGrid txt = checkText
  where
    checkText = checkHead (fst unconsTxt)

    checkHead :: Char -> CustomIdentGrid
    checkHead c
      | isNumber c = error "Custom-ident cannot start with a number"
      | c == '-'   = checkSecond second
      | otherwise  = CustomIdentGrid txt

    checkSecond (Just s)
      | s == '-' =
          error "Custom-ident cannot start with two hyphens"
      | isNumber s =
          error "Custom-ident cannot start with a hyphen followed by a number"
      | otherwise = CustomIdentGrid txt
    checkSecond Nothing = CustomIdentGrid txt

    unconsTxt = case T.uncons txt of
      Nothing -> error "Custom-ident cannot be empty"
      Just (f, r) -> (f, r)

    second :: Maybe Char
    second = fst <$> T.uncons (snd unconsTxt)

-- $invalidValues
--
-- #partial#
-- The below functions are partial. They will raise an error if
-- provided with a @grid-line@ value which is:
--
-- * an 'Integer' value of 0
-- * a pair with an 'Integer' component of value 0
-- * a 'span_' function provided with an 'Integer' value of 0 or negative
-- * a 'span_' function provided with a pair value with
-- an 'Integer' component of 0 or negative.

-- | Property shorthand specifies a grid item's size and location
-- within a grid.
--
-- One to four @grid-line@ values can be specified.
-- Grid-line values must be separated by a '(//)' operator.
--
-- __WARNING:__ this function is partial. See above "Clay.Grid#partial".
--
-- ==== __Examples__
--
-- The below examples assume that the @ExtendedDefaultRules@ GHC language
-- pragma is enabled. See above "Clay.Grid#pragma".
--
-- > gridArea (auto :: GridLine)
--
-- > gridArea "somegridarea"
--
-- > gridArea $ ("somegridarea", 4) // ("someothergridarea", 2)
--
-- > gridArea $ 1 // 3 // 4
gridArea :: ToGridLines4 a => a -> Css
gridArea x = key "grid-area" (partialToGridLines4 x)

-- | Property shorthand specifies a grid item's size and location
-- within a grid column.
--
-- __WARNING:__ this function is partial. See above "Clay.Grid#partial".
--
-- ==== __Examples__
--
-- The below examples assume that the @ExtendedDefaultRules@ GHC language
-- pragma is enabled. See above "Clay.Grid#pragma".
--
-- > gridColumn (auto :: GridLine)
--
-- > gridColumn $ span_ 3
--
-- > gridColumn $ span_ ("somegridarea", 5)
--
-- > gridColumn $ span_ 3 // 6
gridColumn :: ToGridLines2 a => a -> Css
gridColumn x = key "grid-column" (partialToGridLines2 x)

-- | Property specifies a grid item's start position within the grid column.
--
-- __WARNING:__ this function is partial. See above "Clay.Grid#partial".
--
-- ==== __Examples__
--
-- The below examples assume that the @ExtendedDefaultRules@ GHC language
-- pragma is enabled. See above "Clay.Grid#pragma".
--
-- > gridColumnStart (inherit :: GridLine)
--
-- > gridColumnStart 2
--
-- > gridColumnStart ("somegridarea", 4)
--
-- > gridColumnStart $ span_ ("somegridarea", 5)
gridColumnStart :: ToGridLine a => a -> Css
gridColumnStart x = key "grid-column-start" (partialToGridLine x)

-- | Property specifies a grid item's end position within the grid column.
--
-- __WARNING:__ this function is partial. See above "Clay.Grid#partial".
--
-- ==== __Examples__
--
-- The below examples assume that the @ExtendedDefaultRules@ GHC language
-- pragma is enabled. See above "Clay.Grid#pragma".
--
-- > gridColumnEnd (initial :: GridLine)
--
-- > gridColumnEnd 2
--
-- > gridColumnEnd "somegridarea"
--
-- > gridColumnEnd $ span_ "somegridarea"
gridColumnEnd :: ToGridLine a => a -> Css
gridColumnEnd x = key "grid-column-end" (partialToGridLine x)

-- | Property shorthand specifies a grid item's size and location
-- within a grid row.
--
-- One or two @grid-line@ values can be specified.
-- @grid-line@ values must be separated by a '(//)' operator.
--
-- __WARNING:__ this function is partial. See above "Clay.Grid#partial".
--
-- ==== __Examples__
--
-- The below examples assume that the @ExtendedDefaultRules@ GHC language
-- pragma is enabled. See above "Clay.Grid#pragma".
--
-- > gridRow (unset :: GridLine)
--
-- > gridRow $ span_ 3
--
-- > gridRow $ span_ 3 // 6
--
-- > gridRow $ span_ ("somegridarea", 5) // span_ 2
gridRow :: ToGridLines2 a => a -> Css
gridRow x = key "grid-row" (partialToGridLines2 x)

-- | Property specifies a grid item's start position within the grid row.
--
-- __WARNING:__ this function is partial. See above "Clay.Grid#partial".
--
-- ==== __Examples__
--
-- The below examples assume that the @ExtendedDefaultRules@ GHC language
-- pragma is enabled. See above "Clay.Grid#pragma".
--
-- > gridRowStart (initial :: GridLine)
--
-- > gridRowStart (-2)
--
-- > gridRowStart $ span_ "somegridarea"
--
-- > gridRowStart "somegridarea"
gridRowStart :: ToGridLine a => a -> Css
gridRowStart x = key "grid-row-start" (partialToGridLine x)

-- | Property specifies a grid item's end position within the grid row.
--
-- __WARNING:__ this function is partial. See above "Clay.Grid#partial".
--
-- ==== __Examples__
--
-- The below examples assume that the @ExtendedDefaultRules@ GHC language
-- pragma is enabled. See above "Clay.Grid#pragma".
--
-- > gridRowEnd (auto :: GridLine)
--
-- > gridRowEnd (-2)
--
-- > gridRowEnd ("somegridarea", 4)
--
-- > gridRowEnd $ span_ 3
gridRowEnd :: ToGridLine a => a -> Css
gridRowEnd x = key "grid-row-end" (partialToGridLine x)

class Slash a r | a -> r where
  -- | `/` CSS operator.
  -- Separates @grid-line@ values.
  (//) :: ToGridLine b => a -> b -> r

instance Slash GridLine TwoGridLines where
  x // y = TwoGridLines x (toGridLine y)

instance Slash Integer TwoGridLines where
  x // y = TwoGridLines (toGridLine x) (toGridLine y)

instance Slash CustomIdentGrid TwoGridLines where
  x // y = TwoGridLines (toGridLine x) (toGridLine y)

instance Slash String TwoGridLines where
  x // y = TwoGridLines (toGridLine x) (toGridLine y)

instance Slash (CustomIdentGrid, Integer) TwoGridLines where
  x // y = TwoGridLines (toGridLine x) (toGridLine y)

instance Slash (String, Integer) TwoGridLines where
  x // y = TwoGridLines (toGridLine x) (toGridLine y)

instance Slash TwoGridLines ThreeGridLines where
  (TwoGridLines xx xy) // y = ThreeGridLines xx xy (toGridLine y)

instance Slash ThreeGridLines FourGridLines where
  (ThreeGridLines xx xy xz) // y = FourGridLines xx xy xz (toGridLine y)

class ToSpan a where

  -- | @span@ CSS keyword, contributes to the grid item's placement.
  span_ :: a -> GridLine

-- | Contributes the nth grid line to the grid item's placement.
--
-- __NOTE:__ negative 'Integer' or 0 values are invalid.
instance ToSpan Integer where
  span_ x = Span Nothing (Just x)

-- | One line from the provided name is counted.
instance ToSpan String where
  span_ x = Span (Just . CustomIdentGrid $ T.pack x) Nothing

-- | Nth lines from the provided name are counted.
--
-- __NOTE:__ negative 'Integer' or 0 values are invalid.
instance ToSpan (String, Integer) where
  span_ (x, y) = Span (Just . CustomIdentGrid $ T.pack x) (Just y)

-- | Keyword indicating that the property contributes nothing to the grid item's
-- placement.
instance Com.Auto GridLine where
  auto = Auto

-- | Keyword `inherit` applied to a 'GridLine'.
instance Com.Inherit GridLine where
  inherit = Inherit

-- | Keyword `initial` applied to a 'GridLine'.
instance Com.Initial GridLine where
  initial = Initial

-- | Keyword `unset` applied to a 'GridLine'.
instance Com.Unset GridLine where
  unset = Unset

-- | Convertion of 'GridLine' to 'Clay.Property.Value'.
instance Val GridLine where
  value Auto               = "auto"
  value Inherit            = "inherit"
  value Initial            = "initial"
  value Unset              = "unset"
  value (Coordinate x)     = value x
  value (GridLineCustomIdent x y) = value $ customIdentToText x <> foldMap ((" " <>) . tshow) y
  value (Span x y) =
    value $ "span" <> foldMap ((" " <>) . customIdentToText) x <> foldMap ((" " <>) . tshow) y

instance Val OneGridLine where
  value (OneGridLine x) = value x

instance Val TwoGridLines where
  value (TwoGridLines x y) =
    value x <> value (" / " :: Text) <> value y

instance Val ThreeGridLines where
  value (ThreeGridLines x y z) =
       value x
    <> value (" / " :: Text) <> value y
    <> value (" / " :: Text) <> value z

instance Val FourGridLines where
  value (FourGridLines xx xy xz yx) =
       value xx
    <> value (" / " :: Text) <> value xy
    <> value (" / " :: Text) <> value xz
    <> value (" / " :: Text) <> value yx

instance Val GridLines2 where
  value (One2 x) = value x
  value (Two2 x) = value x

instance Val GridLines4 where
  value (One4 x)   = value x
  value (Two4 x)   = value x
  value (Three4 x) = value x
  value (Four4 x)  = value x

-- | Private partial function checking a 'GridLine'.
--
-- An error is raised when:
-- - An 'Integer' value of 0 is provided.
-- - A negative 'Integer' value is provided for a 'Span' constructor.
--
-- Otherwise, the initially provided 'GridLine' value is returned.
partialCheckGridLine :: GridLine -> GridLine
partialCheckGridLine gridLine = case gridLine of
  Coordinate 0                   -> errorValue 0
  GridLineCustomIdent _ (Just 0) -> errorValue 0
  s@(Span _ (Just n))            -> if n < 1
                                    then errorValue n
                                    else s
  _                              -> gridLine
  where
    errorValue n = error ("Value " ++ show n ++ " is invalid")

-- | Private partial function checking 'OneGridLine'.
--
-- An error is raised if an invalid value is found, see 'partialCheckGridLine'.
partialCheckOneGridLine :: OneGridLine -> OneGridLine
partialCheckOneGridLine (OneGridLine gridLine) =
  OneGridLine (partialCheckGridLine gridLine)

-- | Private partial function checking 'TwoGridLines'.
--
-- An error is raised if an invalid value is found, see 'partialCheckGridLine'.
partialCheckTwoGridLines :: TwoGridLines -> TwoGridLines
partialCheckTwoGridLines (TwoGridLines gridLine1 gridLine2) =
  TwoGridLines (partialCheckGridLine gridLine1) (partialCheckGridLine gridLine2)

-- | Private partial function checking 'ThreeGridLines'.
--
-- An error is raised if an invalid value is found, see 'partialCheckGridLine'.
partialCheckThreeGridLines :: ThreeGridLines -> ThreeGridLines
partialCheckThreeGridLines (ThreeGridLines gridLine1 gridLine2 gridLine3) =
  ThreeGridLines
    (partialCheckGridLine gridLine1)
    (partialCheckGridLine gridLine2)
    (partialCheckGridLine gridLine3)

-- | Private partial function checking 'FourGridLines'.
--
-- An error is raised if an invalid value is found, see 'partialCheckGridLine'.
partialCheckFourGridLines :: FourGridLines -> FourGridLines
partialCheckFourGridLines
  (FourGridLines gridLine1 gridLine2 gridLine3 gridLine4) =
    FourGridLines
      (partialCheckGridLine gridLine1)
      (partialCheckGridLine gridLine2)
      (partialCheckGridLine gridLine3)
      (partialCheckGridLine gridLine4)

-- | Private partial function converting its argument to 'GridLine'.
--
-- An error is raised if an invalid value is found, see 'partialCheckGridLine'.
partialToGridLine :: ToGridLine a => a -> GridLine
partialToGridLine = partialCheckGridLine . toGridLine

-- | Private partial function converting its argument to 'GridLines2'.
--
-- An error is raised if an invalid value is found, see 'partialCheckGridLine'.
partialToGridLines2 :: ToGridLines2 a => a -> GridLines2
partialToGridLines2 x = partialGridLine' gridLines
  where
    gridLines = toGridLines2 x
    partialGridLine' (One2 gl) = One2 (partialCheckOneGridLine gl)
    partialGridLine' (Two2 gl) = Two2 (partialCheckTwoGridLines gl)

-- | Private partial function converting its argument to 'GridLines4'.
--
-- An error is raised if an invalid value is found, see 'partialCheckGridLine'.
partialToGridLines4 :: ToGridLines4 a => a -> GridLines4
partialToGridLines4 x = partialGridLine' gridLines
  where
    gridLines = toGridLines4 x
    partialGridLine' (One4 gl)   = One4 (partialCheckOneGridLine gl)
    partialGridLine' (Two4 gl)   = Two4 (partialCheckTwoGridLines gl)
    partialGridLine' (Three4 gl) = Three4 (partialCheckThreeGridLines gl)
    partialGridLine' (Four4 gl)  = Four4 (partialCheckFourGridLines gl)

-- | Private utility function to show 'Text' instead of 'String'.
tshow :: Show a => a -> Text
tshow = T.pack . show
