{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Prelude (($), IO)

import Element
import Selector
import Rules
import Render

main :: IO ()
main = renderAndPrint $

  body > div + a `rules`
    do "font-size" -: "12px"
       "color"     -: "12px"
       "color"     -: "12px"

    <>

  body > ".section" + a `rules`
    do "font-size" -: "12px"
       "color"     -: "12px"

