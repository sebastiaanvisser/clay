{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base ((@?=))

import Clay.Render (renderWith, compact)
import Data.Monoid

main :: IO ()
main = defaultMain
  [ testCase "mempty produces empty compact CSS" $ renderWith compact [] mempty @?= ""
  ]
