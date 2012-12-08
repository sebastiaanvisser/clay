{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Text (pack)
import Prelude hiding (div)
import Clay hiding (i, s)

main :: IO ()
main = css $
  do logo

  where
    s = 80
    m = 20

    logo = "#logo" ?
      do blocked 400 100 (m * 4 + (m + s) * 3) (m * 4 + (m + s) * 3)

         forM_ [0..3] $ \x ->
           forM_ [0..3] $ \y ->
             do let idx = (pack . show) (1 + y * 4 + x)
                    clr = cycle [ "#78e700", "#00b454", "#ff3900", "#de0052" ] !! fromInteger y
                square idx (m * y + x * (s + m))
                           (m * x + y * (s + m))
                           (clr -. 50 +. (x * 50))

         transform (scaleY 0.6)

         div <?
           do position absolute
              transform (rotateZ (deg 45))


    square i x y c = ".square" `with` nthChild i ?
      do blocked x y s s
         background (hGradient (c -. 100) c)
         "-webkit-box-shadow" -: "0 0 40px rgba(80,40,0, 0.8)"
         "box-shadow" -: "0 0 40px rgba(80,40,0, 0.8)"
         font ( FontOptional (Just bold) Nothing (Just italic)
              , FontMandatory (px 50) (Just (px 60)) ["Helvetica", sansSerif]
              )
         color (setA 80 white)
         "-webkit-box-sizing" -: "border-box"
         "-moz-box-sizing" -: "border-box"
         sym2 padding (px 2) (px 22)

         hover &
           do color white
              div <?
                do background (vGradient (setA 0 white) (setA 100 white))

         div <?
           do position absolute
              background (vGradient (setA 0 white) (setA 40 white))
              borderBottomLeftRadius  (pct 100) (pct 100)
              borderBottomRightRadius (pct  40) (pct  15)
              [left, top, right, bottom] `forM_` ($ 4)

         before &
           do toTheRight m
              background (vGradient (c -. 10) (c -. 60))
              transforms [translate (px 0) (px 10), skew (deg 0) (deg 45)]
              "-webkit-box-shadow" -: "0 0 40px rgba(80,40,0, 1)"
              "box-shadow" -: "0 0 40px rgba(80,40,0, 1)"
         after &
           do toTheBottom m
              background (hGradient (c +. 10) (c +. 80))
              transforms [translate (px 10) (px 0), skew (deg 45) (deg 0)]
              "-webkit-box-shadow" -: "0 0 40px rgba(80,40,0, 1)"
              "box-shadow" -: "0 0 40px rgba(80,40,0, 1)"

    blocked x y w h =
      do position absolute
         left   (px x)
         top    (px y)
         width  (px w)
         height (px h)

toTheRight :: Integer -> Css
toTheRight h =
  do position absolute
     "content" -: "''"
     top    (px 0)
     bottom (px 0)
     right  (px (-h))
     width  (px h)

toTheBottom :: Integer -> Css
toTheBottom h =
  do position absolute
     "content" -: "''"
     left   (px 0)
     right  (px 0)
     bottom (px (-h))
     height (px h)

