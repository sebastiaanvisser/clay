{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, CPP #-}
module Clay.Text
(
-- * Letter and word-spacing.

  letterSpacing
, wordSpacing

-- * Text-rendering.

, TextRendering
, textRendering
, optimizeSpeed, optimizeLegibility, geometricPrecision

-- * Text-shadow.

, textShadow

-- * Text-indent.
-- $text-indent

, TextIndent
, textIndent
, eachLine, hanging
, indent

-- * Text-direction.

, TextDirection
, direction
, ltr
, rtl

-- * Text-align.

, TextAlign
, textAlign
, justify, matchParent, start, end
, alignSide
, alignString

-- * White-space.

, WhiteSpace
, whiteSpace
, pre, nowrap, preWrap, preLine

-- * Text-decoration.

, TextDecoration
, textDecoration
, textDecorationStyle
, textDecorationLine
, textDecorationColor
, underline, overline, lineThrough, blink

-- * Text-transform.

, TextTransform
, textTransform
, capitalize, uppercase, lowercase, fullWidth

-- * Text-overflow.

, TextOverflow
, textOverflow
, overflowClip, overflowEllipsis

-- * Word-break.

, WordBreak
, wordBreak
, breakAll
, keepAll
  
-- * Overflow-wrap (and Word-wrap).

, OverflowWrap
, overflowWrap
, wordWrap
, breakWord
  
-- * Content.

, Content
, content
, contents
, attrContent
, stringContent
, uriContent
, urlContent
, openQuote, closeQuote, noOpenQuote, noCloseQuote

)
where

#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
import Data.String
import Data.Text (Text)

import Clay.Background
import Clay.Border
import Clay.Color
import Clay.Common
import Clay.Property
import Clay.Stylesheet
import Clay.Size

-------------------------------------------------------------------------------

letterSpacing :: Size a -> Css
letterSpacing = key "letter-spacing"

wordSpacing :: Size a -> Css
wordSpacing = key "word-spacing"

-------------------------------------------------------------------------------

newtype TextRendering = TextRendering Value
  deriving (Val, Auto, Inherit, Other)

optimizeSpeed, optimizeLegibility, geometricPrecision :: TextRendering

optimizeSpeed      = TextRendering "optimizeSpeed"
optimizeLegibility = TextRendering "optimizeLegibility"
geometricPrecision = TextRendering "geometricPrecision"

textRendering :: TextRendering -> Css
textRendering = key "text-rendering"

-------------------------------------------------------------------------------

textShadow :: Size a -> Size a -> Size a -> Color -> Css
textShadow x y w c = key "text-shadow" (x ! y ! w ! c)

-------------------------------------------------------------------------------

-- $text-indent
--
-- Supply a length — optionally annotated with @each-line@ or @hanging@ or
-- both, or a global value. It is possible to apply the same annotation
-- multiple times, but it has no defined effect.
--
-- Note browser support is currently (March 2018) non-existent, but the
-- Prince typesetting system supports the syntax.
--
-- === Formal argument syntax
--
-- > <length-percentage> && hanging? && each-line?
-- > where
-- > <length-percentage> = <length> | <percentage>

newtype TextIndent = TextIndent Value
  deriving (Val, Inherit, Initial, Unset, Other)

-- | An internal function that ensures each-line and hanging are processed
-- correctly.
tagTextIndent :: Value -> TextIndent -> TextIndent
tagTextIndent v (TextIndent v0) = TextIndent . value $ (v0, v)

-- | Annotate the supplied 'TextIndent' with @each-line@ or @hanging@ or
-- both.
--
-- > eachLine . hanging . indent $ px 3 :: TextIndent
eachLine, hanging :: TextIndent -> TextIndent

eachLine = tagTextIndent "each-line"
hanging  = tagTextIndent "hanging"

indent :: Size a -> TextIndent
indent = TextIndent . value

textIndent :: TextIndent -> Css
textIndent = key "text-indent"

-------------------------------------------------------------------------------

newtype TextDirection = TextDirection Value
  deriving (Val, Normal, Inherit, Other)

ltr :: TextDirection
ltr = TextDirection "ltr"

rtl :: TextDirection
rtl = TextDirection "rtl"

direction :: TextDirection -> Css
direction = key "direction"

-------------------------------------------------------------------------------

newtype TextAlign = TextAlign Value
  deriving (Val, Normal, Inherit, Other, Center)

justify, matchParent, start, end :: TextAlign

justify     = TextAlign "justify"
matchParent = TextAlign "match-parent"
start       = TextAlign "start"
end         = TextAlign "end"

alignSide :: Side -> TextAlign
alignSide = TextAlign . value

alignString :: Char -> TextAlign
alignString = TextAlign . value . Literal . fromString . return

textAlign :: TextAlign -> Css
textAlign = key "text-align"

-------------------------------------------------------------------------------

newtype WhiteSpace = WhiteSpace Value
  deriving (Val, Normal, Inherit, Other)

whiteSpace :: WhiteSpace -> Css
whiteSpace = key "white-space"

pre, nowrap, preWrap, preLine :: WhiteSpace

pre     = WhiteSpace "pre"
nowrap  = WhiteSpace "nowrap"
preWrap = WhiteSpace "pre-wrap"
preLine = WhiteSpace "pre-line"

-------------------------------------------------------------------------------

newtype TextDecoration = TextDecoration Value
  deriving (Val, None, Inherit, Other)

underline, overline, lineThrough, blink :: TextDecoration

underline   = TextDecoration "underline"
overline    = TextDecoration "overline"
lineThrough = TextDecoration "line-through"
blink       = TextDecoration "blink"

textDecorationLine :: TextDecoration -> Css
textDecorationLine = key "text-decoration-line"

textDecorationColor :: Color -> Css
textDecorationColor = key "text-decoration-color"

textDecoration :: TextDecoration -> Css
textDecoration = key "text-decoration"

textDecorationStyle :: Stroke -> Css
textDecorationStyle = key "text-decoration-style"

-------------------------------------------------------------------------------

newtype TextTransform = TextTransform Value
  deriving (Val, None, Inherit)

capitalize, uppercase, lowercase, fullWidth :: TextTransform

capitalize = TextTransform "capitalize"
uppercase  = TextTransform "uppercase"
lowercase  = TextTransform "lowercase"
fullWidth  = TextTransform "full-width"

textTransform :: TextTransform -> Css
textTransform = key "text-transform"

-------------------------------------------------------------------------------

newtype WordBreak = WordBreak Value
  deriving (Val, Inherit, Initial, Unset, Normal)

breakAll, keepAll :: WordBreak

breakAll = WordBreak "break-all"
keepAll  = WordBreak "keep-all"

wordBreak :: WordBreak -> Css

wordBreak = key "word-break"

-------------------------------------------------------------------------------

newtype OverflowWrap = OverflowWrap Value
  deriving (Val, Inherit, Initial, Unset, Normal)

breakWord :: OverflowWrap

breakWord = OverflowWrap "break-word"

overflowWrap, wordWrap :: OverflowWrap -> Css

wordWrap     = key "word-wrap"
overflowWrap = key "overflow-wrap"

-------------------------------------------------------------------------------

newtype TextOverflow = TextOverflow Value
  deriving (Val, None, Inherit, Initial)

overflowClip, overflowEllipsis :: TextOverflow

overflowClip = TextOverflow "clip"
overflowEllipsis = TextOverflow "ellipsis"

textOverflow :: TextOverflow -> Css
textOverflow = key "text-overflow"

-------------------------------------------------------------------------------

newtype Content = Content Value
  deriving (Val, None, Normal, Inherit, Initial)

attrContent :: Text -> Content
attrContent a = Content ("attr(" <> value a <> ")")

stringContent :: Text -> Content
stringContent = Content . value . Literal

uriContent :: Text -> Content
uriContent u = Content ("uri(" <> value (Literal u) <> ")")

urlContent :: Text -> Content
urlContent u = Content ("url(" <> value (Literal u) <> ")")

openQuote, closeQuote, noOpenQuote, noCloseQuote :: Content

openQuote    = Content "open-quote"
closeQuote   = Content "close-quote"
noOpenQuote  = Content "no-open-quote"
noCloseQuote = Content "no-close-quote"

content :: Content -> Css
content = key "content"

contents :: [Content] -> Css
contents cs = key "content" (noCommas cs)

-- TODO: counters
