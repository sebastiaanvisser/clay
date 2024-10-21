{-# LANGUAGE OverloadedStrings #-}

module Clay.GeometrySpec where

import Clay.Common
import Clay.Geometry
import Clay.Render
import Clay.Stylesheet

import Control.Exception (evaluate)
import qualified Data.Ratio as R
import Data.Text.Lazy

import Test.Hspec

compactRender :: Css -> Text
compactRender = renderWith compact []

spec :: Spec
spec = do
  describe "aspect-ratio" $ do
    it "has ratio" $ do
      compactRender (aspectRatio (2%1)) `shouldBe` "{aspect-ratio:2/1}"
      compactRender (aspectRatio (4%3)) `shouldBe` "{aspect-ratio:4/3}"
      compactRender (aspectRatio (8%6)) `shouldBe` "{aspect-ratio:4/3}"

    it "has rational ratio" $ do
      compactRender (aspectRatio $ fromRational $ 2 R.% 1) `shouldBe` "{aspect-ratio:2/1}"
      compactRender (aspectRatio $ fromRational $ 4 R.% 3) `shouldBe` "{aspect-ratio:4/3}"
      compactRender (aspectRatio $ fromRational $ 8 R.% 6) `shouldBe` "{aspect-ratio:4/3}"

    it "has auto value" $ do
      compactRender (aspectRatio auto) `shouldBe` "{aspect-ratio:auto}"

    it "has inherit value" $ do
      compactRender (aspectRatio inherit) `shouldBe` "{aspect-ratio:inherit}"

    it "has initial value" $ do
      compactRender (aspectRatio initial) `shouldBe` "{aspect-ratio:initial}"

    it "has unset value" $ do
      compactRender (aspectRatio unset) `shouldBe` "{aspect-ratio:unset}"

    it "has auto value and fallback ratio" $ do
      compactRender (aspectRatio $ auto `withFallback` (4%3)) `shouldBe` "{aspect-ratio:auto 4/3}"
      compactRender (aspectRatio $ (4%3) `withFallback` auto) `shouldBe` "{aspect-ratio:4/3 auto}"

    it "does not allow invalid fallbacks" $ do
      evaluate (compactRender $ aspectRatio $ auto `withFallback` auto) `shouldThrow` anyErrorCall
      evaluate (compactRender $ aspectRatio $ (4%3) `withFallback` (4%3)) `shouldThrow` anyErrorCall

    it "has arbitrary other value" $ do
      compactRender (aspectRatio $ other "not valid") `shouldBe` "{aspect-ratio:not valid}"
