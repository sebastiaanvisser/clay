{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude (($), IO)

import qualified Prelude

import Color
import Element
import Render
import Rule
import Selector
import Size
import Style


main :: IO ()
main = css $

  do body > div + a ?
       do color             (rgba 255 0 128 128)
          border            solid (px 10) red
          border_left_color green
          sym margin        (px 10) (px 20)

          abbr ? color green

     body > ".section" + a ?
       do "font-size" -: px 12
          color         green

