module Common where

import Clay.Render (renderWith, compact)
import Test.Hspec
import Clay
import Data.Monoid ((<>))
import Data.Text.Lazy (Text, unpack)

shouldRenderFrom :: Text -> Css -> SpecWith ()
shouldRenderFrom txt css = let s = shouldBe . renderWith compact [] in
    it ("renders " <> unpack txt) $ css `s` txt
infixr 0 `shouldRenderFrom`
