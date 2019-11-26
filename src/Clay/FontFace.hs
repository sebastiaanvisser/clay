{-# LANGUAGE OverloadedStrings #-}
module Clay.FontFace
  ( FontFaceFormat (..)
  , FontFaceSrc (..)
  , fontFaceSrc
  ) where

import Clay.Common (call)
import Clay.Property (Prefixed (Plain), Value(Value), Val (value), quote)
import Clay.Stylesheet (Css, key)

import Data.Maybe (fromMaybe)
import Data.Text (Text)

-------------------------------------------------------------------------------

data FontFaceFormat
  = WOFF
  | WOFF2
  | TrueType
  | OpenType
  | EmbeddedOpenType
  | SVG
  deriving Show

-- | name of format according to CSS specification
formatName :: FontFaceFormat -> Text
formatName format = case format of
  WOFF             -> "woff"
  WOFF2            -> "woff2"
  TrueType         -> "truetype"
  OpenType         -> "opentype"
  EmbeddedOpenType -> "embedded-opentype"
  SVG              -> "svg"

-------------------------------------------------------------------------------

data FontFaceSrc
  = FontFaceSrcUrl Text (Maybe FontFaceFormat)
  | FontFaceSrcLocal Text
  deriving Show

instance Val FontFaceSrc where
  value src = Value $ Plain $ case src of
    FontFaceSrcLocal name      -> call "local" (quote name)
    FontFaceSrcUrl url mformat ->
      call "url" (quote url)
      <> fromMaybe "" (call "format" . quote . formatName <$> mformat)

-------------------------------------------------------------------------------

fontFaceSrc :: [FontFaceSrc] -> Css
fontFaceSrc = key "src"
