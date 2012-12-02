{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Prelude hiding (div)
import Clay

main :: IO ()
main = css $
  do overal
     theFooter


overal :: Css
overal =
  do (body <> html) ?
       do height (pct 100)
          resetSpacing

     body ?
       do backgroundColor (grayish 220)
          fonts

          "#container" ?
            do minHeight (pct 100)
               minWidth  (pct 100)


fonts :: Css
fonts =
  do fontFamily "Helvetica, sans-serif"
     fontColor  (rgb 40 30 0)

theFooter :: Css
theFooter =
  footer ?
    do backgroundColor "#ff8800"
       color           white
       position        Absolute
       bottom          (px 0)
       width           (pct 100)
       height          (px 200)

resetSpacing :: Css
resetSpacing =
  do sym4 margin  (px 0)
     sym4 padding (px 0)

