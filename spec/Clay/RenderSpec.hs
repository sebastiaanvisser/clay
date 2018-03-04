{-# LANGUAGE OverloadedStrings #-}
module Clay.RenderSpec where

import Clay.Render (renderWith, compact, htmlInline)
import Test.Hspec
import Clay
import Clay.Stylesheet (comment)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)

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
    describe "compact comment" $ do
        it "with mempty produces no annotation" $ do
            renderWith compact [] (mempty `comment` display displayNone) `shouldBe` "{display:none}"
        it "with comment produces no comment" $ do
            renderWith compact [] ("test" `comment` display displayNone) `shouldBe` "{display:none}"
    describe "pretty comment" $ do
        it "with no comment produces no annotation" $ do
            renderWith pretty [] (display displayNone) `shouldBe`
                withBanner "\n{\n  display : none;\n}\n\n\n"
        it "with mempty produces empty annotation" $ do
            renderWith pretty [] (mempty `comment` display displayNone) `shouldBe`
                withBanner "\n{\n  display : none /*  */;\n}\n\n\n"
        it "with comment produces no comment" $ do
            renderWith pretty [] ("test" `comment` display displayNone) `shouldBe`
                withBanner "\n{\n  display : none /* test */;\n}\n\n\n"

withBanner :: Text -> Text
withBanner = (<> "/* Generated with Clay, http://fvisser.nl/clay */")
