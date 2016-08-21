{-#LANGUAGE OverloadedStrings#-}

module Clay.SizeSpec where

import Clay.Size
import Clay.Property
import Clay.Common

import Test.Hspec
import Data.Text
import Data.List

sizeRepr :: Size a -> Text
sizeRepr = plain . unValue . value

hasAllPrefixes :: Val a => a -> Bool
hasAllPrefixes a = checkPrefixed ((unValue . value) a) browsers
  where checkPrefixed (Prefixed pa) (Prefixed pb) = sort (fmap fst pa) == sort (fmap fst pb)
        checkPrefixed _ _ = False

spec :: Spec
spec = do
  describe "simple sizes" $ do
    it "returns 1px for (px 1)" $
      sizeRepr (px 1) `shouldBe` "1px"
    it "return 50% for (pct 50)" $
      sizeRepr (pct 50) `shouldBe` "50%"
  describe "calc addition" $ do
    it "returns proper calc for simple sum" $
      sizeRepr (em 2 @+@ px 1) `shouldBe` "calc(2em + 1px)"
    it "returns calc for nested sum" $
      sizeRepr (em 2 @+@ pt 1 @+@ px 3) `shouldBe` "calc((2em + 1pt) + 3px)"
    it "returns prefixed calc for simple sum" $
      (em 2 @+@ pt 2) `shouldSatisfy` hasAllPrefixes
    it "return calc for sum of different types" $
      sizeRepr (em 2 @+@ pct 10) `shouldBe` "calc(2em + 10%)"
    it "returns calc for simple difference" $
      sizeRepr (em 2 @-@ px 5) `shouldBe` "calc(2em - 5px)"
    it "returns calc for combination of sum and difference" $
      sizeRepr (em 2 @-@ px 5 @+@ pt 3) `shouldBe` "calc((2em - 5px) + 3pt)"
    it "returns calc for simple multiplication" $
      sizeRepr (3 *@ em 2) `shouldBe` "calc(3 * 2em)"
    it "returns calc for reversed multiplication" $
      sizeRepr (em 2 @* 3) `shouldBe` "calc(3 * 2em)"
    it "returns calc for multiplication with sum" $
      sizeRepr (em 2 @* 3 @+@ px 3) `shouldBe` "calc((3 * 2em) + 3px)"
    it "returns calc for multiplication with difference" $
      sizeRepr (vmax 5 @-@ em 2 @* 6) `shouldBe` "calc(5vmax - (6 * 2em))"
    it "behaves correctly with negatives" $
      sizeRepr (vmax (-5) @-@ em (-2) @* (-6)) `shouldBe` "calc(-5vmax - (-6 * -2em))"
