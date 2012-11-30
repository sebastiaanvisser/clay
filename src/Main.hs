{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Prelude hiding (div)

import Color
import Html
import Render
import Rule
import Selector
import Size
import Style


main :: IO ()
main = css $

  rule body $
    do color             green
       border            solid (px 10) red
       borderLeftColor green

       rule (div <> abbr) $
         do color red
            sym margin (px 10) (px 20)

            self hover $
              color green

            pop 2 $
              do color purple
                 font "Arial, Helvetica, sans-serif" (pt 12) black

            root (html `with` ".open") $
              marginLeft (px 1)

            rule ("#content" <> q) $
              do color green
                 "-webkit-box-shadow" -: "10px 10px 0px rgba(12,12,23)"

