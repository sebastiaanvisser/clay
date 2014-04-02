{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base ((@?=))

import Clay.Render (renderWith, compact)

main :: IO ()
main = defaultMain
  [ testCase "empty Clay produces empty compact CSS" $ renderWith compact [] (return ()) @?= ""
  ]
