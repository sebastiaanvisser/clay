module Main where

import Data.Monoid
import Prelude (($), IO)

import Selector
import Rules
import Render

main :: IO ()
main = renderAndPrint $

  "body" > ".section" + "a" `rules`
    do "font-size" -: "12px"
       "color"     -: "12px"
       "color"     -: "12px"

    <>

  "body" > ".section" + "a" `rules`
    do "font-size" -: "12px"
       "color"     -: "12px"

