{-# LANGUAGE OverloadedStrings #-}
module Codeblock where

import Prelude hiding (div)
import Clay
import Common (codeFont, sndColor)

codeblocks :: Css
codeblocks =

  do isCode ?

       do -- Quite some spacing at the top and bottom, horizontal scrolling
          -- when needed and a slight shadow.

          boxSizing         borderBox
          sym borderRadius  (px 2)
          overflowX         auto
          sym padding       20
          marginTop         (px 60)
          marginBottom      (px 60)
          boxShadow         0 0 (px 60)
                            (setA 30 black)

          pre ?
            do sym margin 0
               codeFont

          -- Specific styling for different languages.

          ".haskell" & haskell
          ".shell"   & haskell -- for now
          ".css"     & css

     -- Reduce the margin between two consecutive code blocks. One is
     -- probably the output of the other and belong closely together.

     isCode |+ isCode ? marginTop (px (-40))

-- How to recognize code blocks.

isCode :: Selector
isCode = div # ".code"

---------------------------------------------------------------------------

-- Haskell code we highlight with a dark background by default.

haskell :: Css
haskell =
  do background (sndColor -. 150)
     pre ?
       do color (setA 160 white)
          ".Comment"  ? color (setA 170 lime)
          ".ConId"    ? color (sndColor +. 100)
          ".Function" ? color white
          ".Keyword"  ? color (sndColor +. 20)
          ".Number"   ? color (setG 100 orange)
          ".String"   ? color (setG 40 red)
          ".Symbol"   ? color orange

---------------------------------------------------------------------------

-- CSS code we highlight with a lighter background by default and we make
-- the font a bit smaller because the generated CSS tends to grow.

css :: Css
css =
  do backgroundColor none
     pre ?
       do fontSize  (px 14)
          color     "#456"
          ".Number"   ? color red
          ".Property" ? color black
          ".Selector" ? color (sndColor -. 60)
          ".String"   ? color red
          ".Symbol"   ? color (orange -. 60)

