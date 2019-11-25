module Common where

import Test.Hspec
import Clay
import Data.Text.Lazy (Text, unpack)

shouldRenderFrom :: Text -> Css -> SpecWith ()
shouldRenderFrom txt css =
  it ("renders " <> unpack txt) $ txt `shouldRenderItFrom` css
infixr 0 `shouldRenderFrom`

shouldRenderAsFrom :: String -> Text -> Css -> SpecWith ()
shouldRenderAsFrom des txt css =
  it ("renders " <> des) $ txt `shouldRenderItFrom` css
infixr 3 `shouldRenderAsFrom`

shouldRenderItFrom :: Text -> Css -> Expectation
shouldRenderItFrom = flip $ shouldBe . renderWith compact []
infixr 0 `shouldRenderItFrom`
