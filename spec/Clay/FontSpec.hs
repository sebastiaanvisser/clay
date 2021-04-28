{-# LANGUAGE OverloadedStrings #-}
module Clay.FontSpec where

import Test.Hspec
import Clay
import Common

spec :: Spec
spec = do
  describe "font" $ do
    describe "generates proper code" $ do
      "{font:14px \"Helvetica\",sans-serif,serif}"
      `shouldRenderFrom`
      font $ Required (px 14) Nothing ["Helvetica"] [sansSerif, serif]
