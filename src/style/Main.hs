{-# LANGUAGE OverloadedStrings #-}
module Main where

import Clay hiding (i, s, id)
import Control.Monad
import Data.Monoid
import Prelude hiding (div, span)
import System.Environment

import qualified Clay.Media        as Media
import qualified Data.Text.Lazy.IO as Text

import Codeblock
import Common
import Header

-- When running the stylesheet we allow the generation of compacted CSS by
-- using 'compact' as the first argument. Otherwise we dump the default
-- pretty printed version.

main :: IO ()
main =
  do args <- getArgs
     case args of
       "compact" : _
          -> Text.putStr (renderWith compact theStylesheet)
       _  -> putCss theStylesheet

---------------------------------------------------------------------------

theStylesheet :: Css
theStylesheet =

  do -- Overall site-wide styling rules.
     body ?
       do sym margin  0
          sym padding 0 
          ".index" & "#container" ?
            paddingTop 360

     -- The site consists out of a header with a logo and navigation,
     -- several sections and a footer. We have common style rules for the
     -- sections and allow specific styling for the header and footer.

     theSections
     header ? theHeader
     footer ? theFooter

---------------------------------------------------------------------------

-- Generic styling that every section shares.

theSections :: Css
theSections =

  do section ?

       do boxSizing borderBox
          [paddingTop, paddingBottom] `forM_` ($ 60)

          -- Hack to prevent margin collapse between headers and previous
          -- section.
          borderTop solid (px 1) transparent

          div <?
            do -- Horizontally center the content div inside a section.
               centered

               -- Allow a bit of spacing even when the screen is small.
               sym2 padding 0 (px 10)

          -- Add some style aspects to the sections.
          ".one-col" ? oneColumn
          ".two-col" ? oneOrTwoColumns
          codeblocks
          textblocks

     -- Every odd section (or the footer) gets a slightly distinguished
     -- background color.

     (section <> footer) # nthChild "odd" ?
       backgroundColor trdColor

-- One and two column layouts.

oneColumn :: Css
oneColumn =
  do width (px 550)
     boxSizing borderBox

oneOrTwoColumns :: Css
oneOrTwoColumns =
  do query Clay.all [Media.minWidth 800] twoColumns
     query Clay.all [Media.maxWidth 800] oneColumn

twoColumns :: Css
twoColumns =

  do -- Both columns are have the size of their parent.
     div <?
       do width      (pct 50)
          boxSizing  borderBox

     -- Float first child to the left, second to the right.
     column "1" floatLeft  paddingRight
     column "2" floatRight paddingLeft

     -- Don't float outside the section.
     br ? clear both

  where column i side pad =
          div # nthChild i <?
            do float  side
               pad    (px 30)

---------------------------------------------------------------------------

-- Content blocks containing running texts.

textblocks :: Css
textblocks = ".text" ?

  do textFont
     anchors

     -- Small caps for anchors.
     h3 ?
       do textTransform  uppercase
          color          (sndColor -. 80)
          fontWeight     bold

     -- A bit less padding for lists.
     ul ? paddingLeft (px 20)

     -- Inline code snippets.
     code ? color "#ff4422"

     -- Slightly indent the goto links.
     ".goto" ? paddingLeft (px 25)

---------------------------------------------------------------------------

theFooter :: Css
theFooter = div <?
  do centered
     width          (px 550)
     textFont
     fontSize       (px 12)
     textTransform  uppercase
     textAlign      (alignSide sideCenter)
     color          (setA 150 black)
     sym2 padding   (px 10) 0

