{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Prelude hiding (div)
import Clay

main :: IO ()
main = css $
  do overal
     theFooter

  where

    overal =
      do body <> html ?
           do height (pct 100)
              resetSpacing

         body <> section ?
           do backgroundColor (grayish 220)
              fonts

              ("href" $= "/page") &
                color (rgba 255 128 0 80)

              hover &
                  backgroundColor green

              "#container" ?
                do minHeight (pct 100)
                   minWidth  (pct 100)

    fonts =
      do fontFamily "Helvetica, sans-serif"
         fontColor  (rgb 40 30 0)

    theFooter =
      footer ?
        do backgroundColor "#ff880020"
           color           white
           position        Absolute
           bottom          (px 0)
           width           (pct 100)
           height          (px 200)
           abbr <? p ? color red

           a ?
             display inline

    resetSpacing =
      do sym4 margin  (px 0)
         sym4 padding (px 0)

