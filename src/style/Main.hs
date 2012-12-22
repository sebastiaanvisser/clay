{-# LANGUAGE OverloadedStrings #-}
module Main where

import Clay hiding (i, s, id)
import Control.Monad
import Data.Monoid
import Prelude hiding (div, span)
import System.Environment

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
          -> Text.putStr (renderWith compact [] theStylesheet)
       _  -> putCss theStylesheet

----------------------------------------------------------------------------

theStylesheet :: Css
theStylesheet =

  do -- Overall site-wide styling rules.
     body ?
       do textFont
          sym margin  0
          sym padding 0 
          ".index" & "#container" ?
            paddingTop 360

     -- The site consists out of a header with a logo and navigation,
     -- several sections and a footer. We have common style rules for the
     -- sections and allow specific styling for the header and footer.

     theSections
     header ? theHeader
     footer ? theFooter

----------------------------------------------------------------------------

-- Generic styling that every section shares.

theSections :: Css
theSections =

  do section ?

       do boxSizing borderBox
          [paddingTop, paddingBottom] `forM_` ($ 60)
          minHeight (px 400)

          -- Hack to prevent margin collapse between headers and previous
          -- section.
          borderTop solid (px 1) transparent

          -- Horizontally center the content div inside a section.
          div <? centered

          -- Add some style aspects to the sections.
          oneColumn
          twoColumns
          codeblocks
          textblocks

     -- Every odd section (or the footer) gets a slightly distinguished
     -- background color.

     (section <> footer) # nthChild "odd" ?
       backgroundColor trdColor

-- One and two column layouts.

oneColumn :: Css
oneColumn = ".one-col" ?
  do width      (px 550)
     boxSizing  borderBox

twoColumns :: Css
twoColumns = ".two-col" ?

  do -- Both columns are have the size of their parent.
     div <?
       do width      (pct 50)
          boxSizing  borderBox

     -- Float first child to the left, second to the right.
     column "1" sideLeft  paddingRight
     column "2" sideRight paddingLeft

     -- Don't float outside the section.
     br ? clear both

  where column i side pad =
          div # nthChild i <?
            do float  side
               pad    (px 30)

----------------------------------------------------------------------------

-- Content blocks containing running texts.

textblocks :: Css
textblocks = ".text" ?
  do fontSize   (px 20)
     lineHeight (px 30)

     anchors
--      a # hover ?
--        do color           (sndColor -. 40)

     h3 ?
       do textTransform  uppercase
          color          (sndColor -. 80)
          fontWeight     bold

     ul ? paddingLeft (px 20)

     p ? a ? color sndColor

     code ? color "#ff4422"

     ".goto" ? paddingLeft (px 40)

----------------------------------------------------------------------------

theFooter :: Css
theFooter = div <?
  do centered
     textAlign     (alignSide sideCenter)
     color         (setA 150 black)
     sym2 padding  (px 10) 0

