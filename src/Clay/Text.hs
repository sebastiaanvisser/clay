{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Clay.Text
(
-- * Letter and word-spacing.

  letterSpacing
, wordSpacing

-- * Text-rendering.

, TextRendering
, optimizeSpeed, optimizeLegibility, geometricPrecision
, textRendering

-- * Text-shadow.

, textShadow

-- * Text-indent.

, TextIndent
, eachLine, hanging
, indent
, textIndent

-- * Text-direction.

, TextDirection
, ltr
, rtl
, direction

-- * Text-align.

, TextAlign
, justify, matchParent, start, end
, alignSide
, alignString
, textAlign

-- * White-space.

, WhiteSpace
, whiteSpace
, pre, nowrap, preWrap, preLine

-- * Text-decoration.

, TextDecoration
, underline, overline, lineThrough, blink
, textDecorationLine
, textDecorationColor
, textDecoration
, textDecorationStyle

-- * Text-transform.

, TextTransform
, capitalize, uppercase, lowercase, fullWidth
, textTransform

-- * Content.

, Content
, attrContent
, stringContent
, uriContent
, openQuote, closeQuote, noOpenQuote, noCloseQuote
, content
, contents

)
where

import Data.Monoid
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

newtype TextIndent = TextIndent Value
  deriving (Val, Inherit, Other)

eachLine, hanging :: TextIndent

eachLine = TextIndent "each-line"
hanging  = TextIndent "hanging"

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
  deriving (Val, Normal, Inherit, Other)

justify, matchParent, start, end :: TextAlign

justify     = TextAlign "justify"
matchParent = TextAlign "matchParent"
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
whiteSpace = key "whiteSpace"

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

newtype Content = Content Value
  deriving (Val, None, Normal, Inherit)

attrContent :: Text -> Content
attrContent a = Content ("attr(" <> value a <> ")")

stringContent :: Text -> Content
stringContent = Content . value . Literal

uriContent :: Text -> Content
uriContent u = Content ("uri(" <> value (Literal u) <> ")")

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

