{-# LANGUAGE OverloadedStrings #-}
-- OverloadedLists here causes things like forM_ [0..3] cause type ambiguity
module Main where

import Control.Monad
import Data.Text (pack)
import Data.List.NonEmpty (fromList)
import Prelude
import Clay hiding (i, s, div)
import Clay.Selector (with)

main :: IO ()
main = putCss logo

  where
    s = 80 :: Double
    m = 30 :: Double
    cs = [ "#78e700"
         , "#00b454"
         , "#ff3900"
         , "#de0052"
         ]

    logo = "#logo" ?
      do rectangular 400 100 (m * 4 + (m + s) * 3) (m * 4 + (m + s) * 3)
         position absolute
         boxSizing borderBox
         transform (scaleY 0.5)

         "div" <?
           do position absolute
              transform (rotateZ (deg 45))

         square
         forM_ [0..3] $ \x ->
           forM_ [0..3] $ \y ->
             do let idx = (pack . show) (1 + y * 4 + x)
                    clr = cycle cs !! floor y
                squareI idx (m * y + x * (s + m))
                            (m * x + y * (s + m))
                            (clr -. 50 +. floor (x * 50))

    square = ".square" ?
      do font ( Optional (Just bold) Nothing (Just italic)
              , Required (px 50) (Just (px 60)) ["Helvetica"] [sansSerif]
              )
         color (setA 80 white)
         sym2 padding (px 2) (px 22)
         position absolute
         boxSizing borderBox

         hover &
           do color white
              "div" <? background (vGradient (setA 0 white) (setA 100 white))

         before &
           do toTheRight m
              transforms [translate (px 0) (px $ fromIntegral $ div (floor m) 2), skew (deg 0) (deg 45)]
              boxShadow $ fromList [black `bsColor` shadowWithBlur 0 0 (px 40)]

         after &
           do toTheBottom m
              transforms [translate (px $ fromIntegral $ div (floor m) 2) (px 0), skew (deg 45) (deg 0)]
              boxShadow $ fromList [black `bsColor` shadowWithBlur 0 0 (px 40)]

         "div" <?
           do position absolute
              background (vGradient (setA 0 white) (setA 60 white))
              borderBottomLeftRadius  (pct 100) (pct 100)
              borderBottomRightRadius (pct  40) (pct  15)
              [left, top, right, bottom] `forM_` ($ pct 4)

    squareI i x y c = ".square" `with` nthChild i ?
      do rectangular x y s s
         background (hGradient (c -. 100) c)
         boxShadow $ fromList [c `bsColor` shadowWithBlur 0 0 (px 50)]
         before & background (vGradient (c -. 10) (c -. 60))
         after  & background (hGradient (c +. 10) (c +. 80))

    rectangular x y w h =
      do left   (px x)
         top    (px y)
         width  (px w)
         height (px h)

    toTheRight h =
      do position absolute
         content (stringContent "@")
         overflow hidden
         top    (px 0)
         bottom (px 0)
         right  (px (-h))
         width  (px h)

    toTheBottom h =
      do position absolute
         content (stringContent "!#!")
         overflow hidden
         left   (px 0)
         right  (px 0)
         bottom (px (-h))
         height (px h)

