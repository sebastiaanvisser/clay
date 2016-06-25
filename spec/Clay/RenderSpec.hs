{-# LANGUAGE OverloadedStrings #-}
module Clay.RenderSpec where

import Clay.Render (renderWith, compact)
import Data.Monoid
import Test.Hspec

spec :: Spec
spec = do
    describe "compact" $ do
        it "with mempty produces empty compact CSS" $
            renderWith compact [] mempty `shouldBe` ""
