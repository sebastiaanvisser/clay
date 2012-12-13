#!/usr/bin/env runhaskell -i../src

\begin{code}

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Prelude hiding (div, span)
import Clay hiding (i, s)

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

          header   ? theHeader
          nav      ? theNav
          "#about" ? theAbout

          "#about" <> "#download" <> "#source" ?
            backgroundColor "#f8f8f8"

  where

  sections =
    do boxSizing borderBox

       div <?
         do centerDiv
            minHeight (px 400)
            sym2 padding (px 40) 0
            fontSize (px 18)

       p ?
         do span ? color "#0044aa"
            lineHeight (px 30)

  columns =
    ".two-col" ?
      do div <?
           do width (pct 50)
              boxSizing borderBox
         div `with` nthChild "1" <? float pLeft
         div `with` nthChild "2" <? float pRight
         div `with` nthChild "1" <? backgroundColor (setA 20 red)
         div `with` nthChild "2" <? backgroundColor (setA 20 blue)
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

centerDiv :: Css
centerDiv =
  do width (px 800)
     boxSizing borderBox
     sym2 margin 0 auto

-------------------------------------------------------------------------------

theHeader :: Css
theHeader =
  do sym2 padding (px 60) (px 20)
     background (vGradient mainColor (mainColor +. 20))

     div <?
       do width (px 800)
          boxSizing borderBox
          sym2 margin 0 auto

     h1 <> h3 ?
       do textTransform uppercase
          textAlign (alignSide pCenter)
          sym margin 0

     h1 ?
       do fontSize (px 90)
          color (setA 220 white)
          letterSpacing (em 0.4)
          textShadow 0 0 (px 20) (mainColor -. 10)

     h3 ?
       do fontSize (px 35)
          color (setA 80 black)
          letterSpacing (em 0.3)
          a `with` hover ?
            do color (setA 200 black)
               textShadow 0 0 (px 10) white

theAbout :: Css
theAbout = backgroundColor "#f8f8f8"

theNav :: Css
theNav =
  do fontSize (px 24)

     div <? centerDiv

     sym2 padding 20 0
     textTransform uppercase
     a ?
       do paddingRight (px 43)
          color highlight
          lastOfType & paddingRight (px 0)
          hover      & color black

\end{code}
