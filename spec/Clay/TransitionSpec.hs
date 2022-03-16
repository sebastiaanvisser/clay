{-# LANGUAGE OverloadedStrings #-}
module Clay.TransitionSpec where

import Test.Hspec
import Clay

spec :: Spec
spec = do
  describe "transition" $ do
    describe "generates proper code" $ do
      it "renders cubic-bezier" $
        plain (unValue (value (cubicBezier 1.0 2.0 3.0 4.0)))
        `shouldBe`
        "cubic-bezier(1,2,3,4)"
