{-# LANGUAGE OverloadedStrings #-}
module Clay.RenderSpec where

import Clay.Render (renderWith, compact, htmlInline)
import Test.Hspec
import Clay

spec :: Spec
spec = do
    describe "compact" $ do
        it "with mempty produces empty compact CSS" $
            renderWith compact [] mempty `shouldBe` ""
    describe "htmlInline" $ do
        it "with rules produces compact inline css" $ do
            let css = do background red
                         color white
            renderWith htmlInline [] css `shouldBe` "background:#ff0000;color:#ffffff"
        it "with discarded selectors" $ do
            let css = body ? do background red
                                color white
            renderWith htmlInline [] css `shouldBe` "background:#ff0000;color:#ffffff"
