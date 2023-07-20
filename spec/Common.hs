module Common where

import Test.Hspec
import Clay
import Control.Exception (evaluate)
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

-- | Test succesfull if, after being evaluated, the error call message
-- raised by the 'Css' argument is equal to the provided 'String'.
shouldThrowErrorCall :: Css -> String -> Expectation
shouldThrowErrorCall css txt =
  evaluate (renderWith compact [] css) `shouldThrow` errorCall txt
infixr 0 `shouldThrowErrorCall`
