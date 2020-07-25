module Common
  ( shouldRenderFrom
  , shouldRenderAsFrom
  , shouldRenderItFrom
  , shouldErrorFromRender
  )
  where

import Test.Hspec
import Clay
import Data.Text.Lazy (Text, unpack)
import Control.Exception (evaluate)
import Control.Exception (Exception(..), evaluate)


shouldRenderFrom :: Text -> Css -> SpecWith ()
shouldRenderFrom txt css =
  it ("renders " <> unpack txt) $ txt `shouldRenderItFrom` css
infixr 0 `shouldRenderFrom`

shouldRenderAsFrom :: String -> Text -> Css -> SpecWith ()
shouldRenderAsFrom des txt css =
  it ("renders " <> des) $ txt `shouldRenderItFrom` css
infixr 3 `shouldRenderAsFrom`

shouldRenderItFrom :: Text -> Css -> Expectation
shouldRenderItFrom = flip $ shouldBe . testRender
infixr 0 `shouldRenderItFrom`

testRender :: Css -> Text
testRender = renderWith compact []

shouldErrorFromRender :: (Exception e, Eq e) => e -> Css -> SpecWith ()
shouldErrorFromRender exception css = do
  let errorMsg = show exception
  let rendered = evaluate $! testRender css
  it ("throws " <> errorMsg) $
    (rendered `shouldThrow` (== exception))
