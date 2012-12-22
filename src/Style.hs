{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Monoid
import Prelude hiding (div, span)
import Clay hiding (i, s, id)
import System.Environment

import qualified Data.Text.Lazy.IO as Text

main :: IO ()
main =
  do args <- getArgs
     case args of
       "compact" : _
          -> Text.putStr (renderWith compact [] site)
       _  -> putCss site

----------------------------------------------------------------------------

site :: Css
site =
  do body ?
       do defaultFont
          sym margin  0
          sym padding 0

          ".index" & "#container" ? paddingTop 360

          section ?
            do sections
               columns

          header     ? theHeader
          nav        ? theNav
          "#about"   ? theAbout
          "#example" ? theExample
          "#source"  ? theSource
          "#haskell" ? theHaskell
          footer     ? theFooter

          (section <> footer) # nthChild "odd" ?
            backgroundColor "#f8f8f8"

  where

  sections =
    do boxSizing borderBox
       borderTop solid (px 1) transparent
       [paddingTop, paddingBottom] `forM_` ($ 60)
       minHeight (px 400)

       codeBlock

       div <?
         do centered

       ".text" ?
         do fontSize (px 20)
            lineHeight (px 30)

            a # hover ?
              do textDecoration underline
                 color (highlight -. 40)

            h4 ?
              do textTransform uppercase
                 color (highlight -. 80)
                 fontWeight bold

            ul ? paddingLeft (px 20)

            p ?
               do a ? color highlight

            code ? color "#ff4422"

            ".goto" ? paddingLeft (px 40)

  columns =
    do ".one-col" ?
         do width (px 550)
            boxSizing borderBox

       ".two-col" ?
         do div <?
              do width (pct 50)
                 boxSizing borderBox
            div # nthChild "1" <?
              do float sideLeft
                 paddingRight (px 30)
            div # nthChild "2" <?
              do float sideRight
                 paddingLeft (px 30)
            br ? clear both

----------------------------------------------------------------------------

mainColor :: Color
mainColor = rgb 255 160 50

highlight :: Color
highlight = rgb 56 135 190

defaultFont :: Css
defaultFont =
  do fontFamily ["Europa", "Helvetica", sansSerif]
     textRendering optimizeLegibility
     color "#222"
     a ?
       do textDecoration none
          color inherit

centered :: Css
centered =
  do width (px 800)
     boxSizing borderBox
     sym2 margin 0 auto

----------------------------------------------------------------------------
-- Styling for specific sections.

theHeader :: Css
theHeader =
  do background (vGradient (mainColor -. 60) (mainColor +. 20))
     position fixed
     top    (px 0)
     left   (px 0)
     right  (px 0)
     height (px 288)

     before &
       do position absolute
          top    (px 0)
          bottom (px 0)
          left   (px 0)
          right  (px 0)
          content (stringContent "")
          pointerEvents none
          backgroundSize (pct 100 `by` px 5)
          backgroundImage $
            repeatingLinearGradient (straight sideTop)
              [ ( setA  0 white,   0)
              , ( setA 20 white,  50)
              , ( setA  0 white, 100)
              ]

     div <?
       do centered
          paddingTop (px 60)
          paddingBottom (px 60)
          height (pct 100)
          overflow hidden

          backgroundImage $
            radialGradient
              sideCenter
              ellipse
              [ ( setA 150 yellow ,  0 )
              , ( setA  25 yellow , 50 )
              , ( setA   0 yellow , 65 )
              ]

          h1 <> h3 ?
            do textTransform uppercase
               textAlign (alignSide sideCenter)
               sym margin 0

          h1 ?
            do fontSize (px 90)
               color (setA 200 white)
               id                 letterSpacing (em 0.40)
               span # ".a" ? letterSpacing (em 0.36)
               span # ".y" ? letterSpacing (em 0.00)
               textShadow 0 0 (px 20) (setA 200 (mainColor -. 80))
               a ? hover &
                 do color white
                    textShadow 0 0 (px 20) (mainColor -. 120)

          h3 ?
            do fontSize (px 35)
               color (setA 120 black)
               letterSpacing (em 0.3)
               a # hover ? color (setA 220 black)

theAbout :: Css
theAbout = return ()

theNav :: Css
theNav =
  do background (setA 251 white)
     boxShadow 0 0 (px 60) (setA 20 black)
     fontSize (px 24)
     sym2 padding 20 0
     textTransform uppercase
     position absolute
     left 0
     right 0
     bottom (px (-72))

     div <? centered
     a ? do paddingRight (px 45)
            lastOfType & paddingRight (px 0)
            color highlight
            hover      & color black

theExample :: Css
theExample = return ()

theSource :: Css
theSource = return ()

theHaskell :: Css
theHaskell = minHeight (px 1000)

theFooter :: Css
theFooter =
  div <?
    do centered
       textAlign (alignSide sideCenter)
       color (setA 150 black)
       sym2 padding (px 10) 0

----------------------------------------------------------------------------
-- Syntax highlighted code blocks.

codeBlock :: Css
codeBlock =

  do ".code" ?
       do boxSizing borderBox
          sym padding 20
          marginTop    (px 60)
          marginBottom (px 60)
          boxShadow 0 0 (px 60) (setA 20 black)
          overflowX auto

          pre ?
            do fontSize   (px 16)
               fontFamily ["Monaco", "Courier New", monospace]
               lineHeight (ex 2.6)
               sym margin 0

     (".code" # ".haskell") <>
       (".code" # ".shell") ?
       do background (highlight -. 150)
          pre ?
            do color (setA 160 white)
               ".Comment"  ? color (rgb 255 60 100)
               ".ConId"    ? color (highlight +. 100)
               ".Function" ? color white
               ".Keyword"  ? color (highlight +. 20)
               ".Number"   ? color (setG 100 orange)
               ".String"   ? color (setG 60 orange)
               ".Symbol"   ? color orange

     ".code" # ".css" ? pre ?
            do fontSize (px 14)
               color "#456"
               ".Number"   ? color red
               ".Property" ? color black
               ".Selector" ? color (highlight -. 60)
               ".String"   ? color red
               ".Symbol"   ? color (orange -. 60)

     ".code" |+ ".code" ?
       do marginTop (px (-40))

