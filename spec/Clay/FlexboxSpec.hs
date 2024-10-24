{-#LANGUAGE OverloadedStrings#-}

module Clay.FlexboxSpec where

import Clay.Common
import Clay.Flexbox
import Clay.Render
import Clay.Stylesheet

import Test.Hspec
import Data.Text.Lazy (Text)

compactRender :: Css -> Text
compactRender = renderWith compact []

spec :: Spec
spec = do
  describe "justify-content" $ do
    it "is inherit" $ do
      (compactRender $ justifyContent inherit) `shouldBe` "{justify-content:inherit}"

    it "is center" $ do
      (compactRender $ justifyContent center) `shouldBe` "{justify-content:center}"

    it "is flex-end" $ do
      (compactRender $ justifyContent flexEnd) `shouldBe` "{justify-content:flex-end}"

    it "is flex-start" $ do
      (compactRender $ justifyContent flexStart) `shouldBe` "{justify-content:flex-start}"

    it "is space-around" $ do
      (compactRender $ justifyContent spaceAround) `shouldBe` "{justify-content:space-around}"

    it "is space-between" $ do
      (compactRender $ justifyContent spaceBetween) `shouldBe` "{justify-content:space-between}"

    it "is space-evenly" $ do
      (compactRender $ justifyContent spaceEvenly) `shouldBe` "{justify-content:space-evenly}"

    it "is stretch" $ do
      (compactRender $ justifyContent stretch) `shouldBe` "{justify-content:stretch}"

    it "is other" $ do
      (compactRender $ justifyContent $ other "random") `shouldBe` "{justify-content:random}"
