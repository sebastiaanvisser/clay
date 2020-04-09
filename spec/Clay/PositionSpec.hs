{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Clay.PositionSpec where

import Test.Hspec
import Clay
import Common

spec :: Spec
spec = do
  describe "position (Mozilla examples)" $ do
    describe "static" $ do
      "{position:static}" `shouldRenderAsFrom`
        "{position:static}" $
          position static
    describe "absolute" $ do
      "{position:absolute}" `shouldRenderAsFrom`
        "{position:absolute}" $
          position absolute
    describe "fixed" $ do
      "{position:fixed}" `shouldRenderAsFrom`
        "{position:fixed}" $
          position fixed
    describe "sticky" $ do
      "{position:sticky}" `shouldRenderAsFrom`
        "{position:-webkit-sticky;position:sticky}" $
          position sticky
