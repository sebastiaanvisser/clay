{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Clay.GridSpec where

import Test.Hspec
import Common
import Clay

spec :: Spec
spec = do
  describe "grid-template-areas" $ do
    describe "mozilla example" $ do
      let
        area_a = "a"
        area_b = "b"
        area_c = "c"
        area_blank = "."

      "{grid-template-areas:\"a a .\"\n\"a a .\"\n\". b c\"}"
        `shouldRenderFrom`
        gridTemplateAreas
          [ [ area_a, area_a, area_blank]
          , [ area_a, area_a, area_blank]
          , [ area_blank, area_b, area_c]
          ]
    describe "non rectangular template areas should error" $ do
      let
        area_a = "a"
        area_b = "b"
        area_c = "c"
        area_blank = "."

      GridTemplateAreas_NotRectangular
        `shouldErrorFromRender`
        gridTemplateAreas
          [ [ area_blank] -- length 1
          , [ area_a, area_blank]         -- length 2
          , [ area_blank, area_b, area_c] -- length 3
          ]

    describe "empty template should error" $ do
      GridTemplateAreas_Empty
        `shouldErrorFromRender`
        gridTemplateAreas []

    describe "template with empty row(s) should error" $ do
      GridTemplateAreas_EmptyRow
        `shouldErrorFromRender`
        gridTemplateAreas [[], []]
