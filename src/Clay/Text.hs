{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Clay.Text where

import Data.Monoid
import Data.Text(Text)

import Clay.Common
import Clay.Property
import Clay.Rule

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

