{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Clay.GridSpec where

import Test.Hspec
import Common
import Clay

spec :: Spec
spec = do
  describe "grid-template-areas" $ do
    describe "keyword values" $ do
      "{grid-template-areas:none}"
        `shouldRenderFrom`
        gridTemplateAreas none

      "{grid-template-areas:inherit}"
        `shouldRenderFrom`
        gridTemplateAreas inherit

      "{grid-template-areas:initial}"
        `shouldRenderFrom`
        gridTemplateAreas initial

      "{grid-template-areas:unset}"
        `shouldRenderFrom`
        gridTemplateAreas unset

    describe "mozilla example" $ do
      let
        area_a = "a"
        area_b = "b"
        area_c = "c"
        area_blank = blankGridArea

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
        area_blank = blankGridArea

      GridTemplateNamedAreas_NotRectangular
        `shouldErrorFromRender`
        gridTemplateAreas
          [ [ area_blank]                 -- length 1
          , [ area_a, area_blank]         -- length 2
          , [ area_blank, area_b, area_c] -- length 3
          ]

    describe "empty template should error" $ do
      GridTemplateNamedAreas_Empty
        `shouldErrorFromRender`
        gridTemplateAreas []

    describe "template with empty row(s) should error" $ do
      GridTemplateNamedAreas_EmptyRow
        `shouldErrorFromRender`
        gridTemplateAreas [[], []]
