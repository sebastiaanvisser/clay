{-# LANGUAGE OverloadedStrings #-}
module Clay.RenderSpec where

import Clay.Render (renderWith, compact, htmlInline)
import Test.Hspec
import Clay
import Clay.Comments
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
    describe "compact ‘commenting’" $ do
        it "with mempty produces no annotation" $ do
            renderWith compact [] (mempty `commenting` display displayNone) `shouldBe` "{display:none}"
        it "with ‘commenting’ produces no comment" $ do
            renderWith compact [] ("test" `commenting` display displayNone) `shouldBe` "{display:none}"
    describe "pretty ‘commenting’" $ do
        it "with no ‘commenting’ produces no annotation" $ do
            renderWith pretty [] (display displayNone) `shouldBe`
                withBanner "\n{\n  display : none;\n}\n\n\n"
        it "with mempty produces empty annotation" $ do
            renderWith pretty [] (mempty `commenting` display displayNone) `shouldBe`
                withBanner "\n{\n  display : none /*  */;\n}\n\n\n"
        it "with ‘commenting’ produces no comment" $ do
            renderWith pretty [] ("test" `commenting` display displayNone) `shouldBe`
                withBanner "\n{\n  display : none /* test */;\n}\n\n\n"

withBanner :: Text -> Text
withBanner = (<> "/* Generated with Clay, http://fvisser.nl/clay */")
