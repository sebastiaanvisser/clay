{-# LANGUAGE OverloadedStrings #-}
module Clay.TextSpec where

import Clay.Render (renderWith, compact, htmlInline, withBanner)
import Test.Hspec
import Clay
import Data.Monoid ((<>))
import Data.Text.Lazy (Text, unpack)

spec :: Spec
spec = do
  describe "text-indent (Mozilla examples)" $ do
    describe "<length> values" $ do
      "{text-indent:3mm}" `shouldRenderFrom`
        textIndent . indent $ mm 3
      "{text-indent:40px}" `shouldRenderFrom`
        textIndent . indent $ px 40

    describe "<percentage> value relative to the containing block width" $ do
      "{text-indent:15%}" `shouldRenderFrom`
        textIndent . indent $ pct 15

    describe "Keyword values" $ do
      "{text-indent:5em each-line}" `shouldRenderFrom`
        textIndent . eachLine . indent $ em 5
      "{text-indent:5em hanging}" `shouldRenderFrom`
        textIndent . hanging . indent $ em 5
      "{text-indent:5em hanging each-line}" `shouldRenderFrom`
        textIndent . eachLine . hanging . indent $ em 5

    describe "Global values" $ do
      "{text-indent:inherit}" `shouldRenderFrom`
        textIndent inherit
      "{text-indent:initial}" `shouldRenderFrom`
        textIndent initial
      "{text-indent:unset}" `shouldRenderFrom`
        textIndent unset

shouldRenderFrom :: Text -> Css -> SpecWith ()
shouldRenderFrom txt css = let s = shouldBe . renderWith compact [] in
    it ("renders " <> unpack txt) $ css `s` txt
infixr 0 `shouldRenderFrom`
