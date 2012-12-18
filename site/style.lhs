#!/usr/bin/env runhaskell -i../src

\begin{code}

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Monoid
import Prelude hiding (div, span)
import Clay hiding (i, s, id)

main :: IO ()
main =
  do putStrLn "Content-Type: text/css\r"
     putStrLn "\r"
     css site

site :: Css
site =
  do body ?
       do defaultFont
          sym margin  0
          sym padding 0

          section ?
            do sections
               columns

          header     ? theHeader
          nav        ? theNav
          "#about"   ? theAbout
          "#example" ? theExample
          "#source"  ? theSource

          "#about" <> "#install" <> "#source" ?
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

            a `with` hover ?
              do textDecoration underline
                 color (highlight -. 40)

            h4 ?
              do textTransform uppercase
                 color (highlight -. 80)
                 fontWeight bold

            ul ? paddingLeft (px 20)

            p ?
               do a ? color highlight

            ".goto" ? paddingLeft (px 40)

  columns =
    ".two-col" ?
      do div <?
           do width (pct 50)
              boxSizing borderBox
         div `with` nthChild "1" <?
           do float pLeft
              paddingRight (px 20)
         div `with` nthChild "2" <?
           do float pRight
              paddingLeft (px 20)
         br ? clear both

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

theHeader :: Css
theHeader =
  do background (vGradient (mainColor -. 60) (mainColor +. 20))
     position fixed
     top (px 0)
     left (px 0)
     right (px 0)

     after &
       do position absolute
          top    (px 0)
          bottom (px 0)
          left   (px 0)
          right  (px 0)
          content (stringContent "")
          "pointer-events" -: "none"
          "background-size" -: "100% 5px"
          "background-image" -:
             ( "-webkit-repeating-linear-gradient(top"
             <> ", rgba(255,255,255,0.00)   0%"
             <> ", rgba(255,255,255,0.08)  50%"
             <> ", rgba(255,255,255,0.00) 100%"
             <> ");"
             )

     div <?
       do centered
          paddingTop (px 60)
          paddingBottom (px 60)
          overflow hidden

          position relative
          before &
            do position absolute
               let m = -80
               top    (px m)
               bottom (px m)
               left   (px 0)
               right  (px 0)
               content (stringContent "")
               "pointer-events" -: "none"
               "background-image" -:
                  ( "-webkit-radial-gradient(center, ellipse"
                  <> ", rgba(255,255,0,0.6) 0%,rgba(255,255,0,0) 65%);"
                  )

     h1 <> h3 ?
       do textTransform uppercase
          textAlign (alignSide pCenter)
          sym margin 0

     h1 ?
       do fontSize (px 90)
          color (setA 210 white)
          id                 letterSpacing (em 0.40)
          span `with` ".a" ? letterSpacing (em 0.36)
          span `with` ".y" ? letterSpacing (em 0.00)
          textShadow 0 0 (px 20) (mainColor -. 60)
          a ? hover &
            do color white
               textShadow 0 0 (px 20) (mainColor -. 120)

     h3 ?
       do fontSize (px 35)
          color (setA 120 black)
          letterSpacing (em 0.3)
          a `with` hover ?
            do color (setA 220 black)
               textShadow 0 0 (px 10) white

theAbout :: Css
theAbout =
  do paddingTop (px 400)

theNav :: Css
theNav =
  do background white
     boxShadow 0 0 (px 60) (setA 20 black)
     fontSize (px 24)
     sym2 padding 20 0
     textTransform uppercase

     div <? centered
     a ? do paddingRight (px 55)
            lastOfType & paddingRight (px 0)
            color highlight
            hover      & color black

theExample :: Css
theExample = return ()

codeBlock :: Css
codeBlock = ".code" ?

    do boxSizing borderBox
       borderRadius 2
       width (px 600)
       sym padding 20
       marginTop (px 60)
       background (vGradient (highlight -. 160) (highlight -. 140))

       pre ?
         do fontSize   (px 16)
            fontFamily ["Monaco", "Courier New", monospace]
            color (setA 160 white)
            ".Function" ? color white
            ".Symbol"   ? color orange
            ".Keyword"  ? color (highlight +. 20)
            ".Number"   ? color (setG 100 orange)
            ".ConId"    ? color (highlight +. 100)

theSource :: Css
theSource =
  minHeight (px 1000)

\end{code}
