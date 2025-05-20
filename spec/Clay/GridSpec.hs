{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Clay.GridSpec where

import Test.Hspec
import Common
import Control.Exception (evaluate)
import Clay

spec :: Spec
spec = do
  describe "grid row start" $ do

    "{grid-row-start:auto}"
      `shouldRenderFrom`
      gridRowStart (auto :: GridLine)

    "{grid-row-start:somegridarea}"
      `shouldRenderFrom`
      gridRowStart "somegridarea"

    "{grid-row-start:somegridarea}"
      `shouldRenderFrom`
      gridRowStart (partialMkCustomIdentGrid "somegridarea")

    "{grid-row-start:2}"
      `shouldRenderFrom`
      gridRowStart 2

    "{grid-row-start:-2}"
      `shouldRenderFrom`
      gridRowStart (-2)

    "{grid-row-start:somegridarea 4}"
      `shouldRenderFrom`
      gridRowStart ("somegridarea", 4)

    "{grid-row-start:span 3}"
      `shouldRenderFrom`
      gridRowStart $ span_ 3

    "{grid-row-start:span -somegridarea}"
      `shouldRenderFrom`
      gridRowStart $ span_ "-somegridarea"

    "{grid-row-start:span somegridarea 5}"
      `shouldRenderFrom`
      gridRowStart $ span_ ("somegridarea", 5)

    "{grid-row-start:inherit}"
      `shouldRenderFrom`
      gridRowStart (inherit :: GridLine)

    "{grid-row-start:initial}"
      `shouldRenderFrom`
      gridRowStart (initial :: GridLine)

    "{grid-row-start:unset}"
      `shouldRenderFrom`
      gridRowStart (unset :: GridLine)

  describe "grid row start errors" $ do

    it "throw error when value is 0" $ do
      gridRowStart 0
        `shouldThrowErrorCall`
        "Value 0 is invalid"

    it "throw error when second value is 0" $ do
      gridRowStart ("somegridarea", 0)
        `shouldThrowErrorCall`
        "Value 0 is invalid"

    it "throw error when custom-ident start with number" $ do
      gridRowStart "0test"
        `shouldThrowErrorCall`
        "Custom-ident cannot start with a number"

    it "throw error when custom-ident start with number in span_" $ do
      gridRowStart (span_ "2test")
        `shouldThrowErrorCall`
        "Custom-ident cannot start with a number"

  describe "grid row end" $ do

    "{grid-row-end:auto}"
      `shouldRenderFrom`
      gridRowEnd (auto :: GridLine)

    "{grid-row-end:somegridarea}"
      `shouldRenderFrom`
      gridRowEnd "somegridarea"

    "{grid-row-end:2}"
      `shouldRenderFrom`
      gridRowEnd 2

    "{grid-row-end:-2}"
      `shouldRenderFrom`
      gridRowEnd (-2)

    "{grid-row-end:somegridarea 4}"
      `shouldRenderFrom`
      gridRowEnd ("somegridarea", 4)

    "{grid-row-end:span 3}"
      `shouldRenderFrom`
      gridRowEnd $ span_ 3

    "{grid-row-end:span somegridarea}"
      `shouldRenderFrom`
      gridRowEnd $ span_ "somegridarea"

    "{grid-row-end:span somegridarea 5}"
      `shouldRenderFrom`
      gridRowEnd $ span_ ("somegridarea", 5)

    "{grid-row-end:inherit}"
      `shouldRenderFrom`
      gridRowEnd (inherit :: GridLine)

    "{grid-row-end:initial}"
      `shouldRenderFrom`
      gridRowEnd (initial :: GridLine)

    "{grid-row-end:unset}"
      `shouldRenderFrom`
      gridRowEnd (unset :: GridLine)

  describe "grid row end errors" $ do

    it "throw error when value is 0" $ do
      gridRowEnd 0
        `shouldThrowErrorCall`
        "Value 0 is invalid"

    it "throw error when second value is 0" $ do
      gridRowEnd ("somegridarea", 0)
        `shouldThrowErrorCall`
        "Value 0 is invalid"

  describe "grid row" $ do

    "{grid-row:auto}"
      `shouldRenderFrom`
      gridRow (auto :: GridLine)

    "{grid-row:auto / auto}"
      `shouldRenderFrom`
      gridRow $ (auto :: GridLine) // (auto :: GridLine)

    "{grid-row:somegridarea}"
      `shouldRenderFrom`
      gridRow "somegridarea"

    "{grid-row:somegridarea / someothergridarea}"
      `shouldRenderFrom`
      gridRow $ "somegridarea" // "someothergridarea"

    "{grid-row:somegridarea 4}"
      `shouldRenderFrom`
      gridRow ("somegridarea", 4)

    "{grid-row:somegridarea 4 / 6}"
      `shouldRenderFrom`
      gridRow $ ("somegridarea", 4) // 6

    "{grid-row:span 3}"
      `shouldRenderFrom`
      gridRow $ span_ 3

    "{grid-row:span somegridarea}"
      `shouldRenderFrom`
      gridRow $ span_ "somegridarea"

    "{grid-row:span somegridarea 5}"
      `shouldRenderFrom`
      gridRow $ span_ ("somegridarea", 5)

    "{grid-row:span 3 / 6}"
      `shouldRenderFrom`
      gridRow $ span_ 3 // 6

    "{grid-row:span somegridarea / span someothergridarea}"
      `shouldRenderFrom`
      gridRow $ span_ "somegridarea" // span_ "someothergridarea"

    "{grid-row:span somegridarea 5 / span 2}"
      `shouldRenderFrom`
      gridRow $ span_ ("somegridarea", 5) // span_ 2

    "{grid-row:inherit}"
      `shouldRenderFrom`
      gridRow (inherit :: GridLine)

    "{grid-row:initial}"
      `shouldRenderFrom`
      gridRow (initial :: GridLine)

    "{grid-row:unset}"
      `shouldRenderFrom`
      gridRow (unset :: GridLine)

  describe "grid row errors" $ do

    it "throw error when value is 0" $ do
      gridRow 0
        `shouldThrowErrorCall`
        "Value 0 is invalid"

    it "throw error when second value is 0" $ do
      gridRow ("somegridarea", 0)
        `shouldThrowErrorCall`
        "Value 0 is invalid"

    it "throw error when span value is 0" $ do
      gridRow (span_ 0)
        `shouldThrowErrorCall`
        "Value 0 is invalid"

    it "throw error when span value is negative" $ do
      gridRow (span_ (-1))
        `shouldThrowErrorCall`
        "Value -1 is invalid"

    it "throw error when second span value is negative" $ do
      gridRow (span_ ("somegridarea", (-1)))
        `shouldThrowErrorCall`
        "Value -1 is invalid"

    it "throw error when second grid value is 0" $ do
      gridRow (1 // 0)
        `shouldThrowErrorCall`
        "Value 0 is invalid"

    -- Must not compile as grid-row only accepts up to 2 arguments.
    -- gridRow $ "nav" // 2 // 3

  describe "grid column start" $ do

    "{grid-column-start:auto}"
      `shouldRenderFrom`
      gridColumnStart (auto :: GridLine)

    "{grid-column-start:somegridarea}"
      `shouldRenderFrom`
      gridColumnStart "somegridarea"

    "{grid-column-start:2}"
      `shouldRenderFrom`
      gridColumnStart 2

    "{grid-column-start:somegridarea 4}"
      `shouldRenderFrom`
      gridColumnStart ("somegridarea", 4)

    "{grid-column-start:span 3}"
      `shouldRenderFrom`
      gridColumnStart $ span_ 3

    "{grid-column-start:span somegridarea}"
      `shouldRenderFrom`
      gridColumnStart $ span_ "somegridarea"

    "{grid-column-start:span somegridarea 5}"
      `shouldRenderFrom`
      gridColumnStart $ span_ ("somegridarea", 5)

    "{grid-column-start:inherit}"
      `shouldRenderFrom`
      gridColumnStart (inherit :: GridLine)

    "{grid-column-start:initial}"
      `shouldRenderFrom`
      gridColumnStart (initial :: GridLine)

    "{grid-column-start:unset}"
      `shouldRenderFrom`
      gridColumnStart (unset :: GridLine)

  describe "grid column end" $ do

    "{grid-column-end:auto}"
      `shouldRenderFrom`
      gridColumnEnd (auto :: GridLine)

    "{grid-column-end:somegridarea}"
      `shouldRenderFrom`
      gridColumnEnd "somegridarea"

    "{grid-column-end:2}"
      `shouldRenderFrom`
      gridColumnEnd 2

    "{grid-column-end:somegridarea 4}"
      `shouldRenderFrom`
      gridColumnEnd ("somegridarea", 4)

    "{grid-column-end:span 3}"
      `shouldRenderFrom`
      gridColumnEnd $ span_ 3

    "{grid-column-end:span somegridarea}"
      `shouldRenderFrom`
      gridColumnEnd $ span_ "somegridarea"

    "{grid-column-end:span somegridarea 5}"
      `shouldRenderFrom`
      gridColumnEnd $ span_ ("somegridarea", 5)

    "{grid-column-end:inherit}"
      `shouldRenderFrom`
      gridColumnEnd (inherit :: GridLine)

    "{grid-column-end:initial}"
      `shouldRenderFrom`
      gridColumnEnd (initial :: GridLine)

    "{grid-column-end:unset}"
      `shouldRenderFrom`
      gridColumnEnd (unset :: GridLine)

  describe "grid column" $ do

    "{grid-column:auto}"
      `shouldRenderFrom`
      gridColumn (auto :: GridLine)

    "{grid-column:auto / auto}"
      `shouldRenderFrom`
      gridColumn $ (auto :: GridLine) // (auto :: GridLine)

    "{grid-column:somegridarea}"
      `shouldRenderFrom`
      gridColumn "somegridarea"

    "{grid-column:somegridarea / someothergridarea}"
      `shouldRenderFrom`
      gridColumn $ "somegridarea" // "someothergridarea"

    "{grid-column:somegridarea 4}"
      `shouldRenderFrom`
      gridColumn ("somegridarea", 4)

    "{grid-column:somegridarea 4 / 6}"
      `shouldRenderFrom`
      gridColumn $ ("somegridarea", 4) // 6

    "{grid-column:span 3}"
      `shouldRenderFrom`
      gridColumn $ span_ 3

    "{grid-column:span somegridarea}"
      `shouldRenderFrom`
      gridColumn $ span_ "somegridarea"

    "{grid-column:span somegridarea 5}"
      `shouldRenderFrom`
      gridColumn $ span_ ("somegridarea", 5)

    "{grid-column:span 3 / 6}"
      `shouldRenderFrom`
      gridColumn $ span_ 3 // 6

    "{grid-column:span somegridarea / span someothergridarea}"
      `shouldRenderFrom`
      gridColumn $ span_ "somegridarea" // span_ "someothergridarea"

    "{grid-column:span somegridarea 5 / span 2}"
      `shouldRenderFrom`
      gridColumn $ span_ ("somegridarea", 5) // span_ 2

    "{grid-column:inherit}"
      `shouldRenderFrom`
      gridColumn (inherit :: GridLine)

    "{grid-column:initial}"
      `shouldRenderFrom`
      gridColumn (initial :: GridLine)

    "{grid-column:unset}"
      `shouldRenderFrom`
      gridColumn (unset :: GridLine)

  describe "grid area" $ do

    "{grid-area:auto}"
      `shouldRenderFrom`
      gridArea (auto :: GridLine)

    "{grid-area:auto / auto}"
      `shouldRenderFrom`
      gridArea $ (auto :: GridLine) // (auto :: GridLine)

    "{grid-area:auto / auto / auto}"
      `shouldRenderFrom`
      gridArea $ (auto :: GridLine) // (auto :: GridLine) // (auto :: GridLine)

    "{grid-area:auto / auto / auto / auto}"
      `shouldRenderFrom`
      gridArea $
           (auto :: GridLine)
        // (auto :: GridLine)
        // (auto :: GridLine)
        // (auto :: GridLine)

    "{grid-area:somegridarea}"
      `shouldRenderFrom`
      gridArea "somegridarea"

    "{grid-area:somegridarea / someothergridarea}"
      `shouldRenderFrom`
      gridArea $ "somegridarea" // "someothergridarea"

    ("{grid-area:somegridarea / someothergridarea /"
      <> " someothergridarea / someothergridarea}")
      `shouldRenderFrom`
      gridArea $ partialMkCustomIdentGrid "somegridarea"
              // partialMkCustomIdentGrid "someothergridarea"
              // partialMkCustomIdentGrid "someothergridarea"
              // partialMkCustomIdentGrid "someothergridarea"

    "{grid-area:somegridarea 4}"
      `shouldRenderFrom`
      gridArea ("somegridarea", 4)

    "{grid-area:somegridarea 4 / someothergridarea 2}"
      `shouldRenderFrom`
      gridArea $ ("somegridarea", 4) // ("someothergridarea", 2)

    "{grid-area:span 3}"
      `shouldRenderFrom`
      gridArea $ span_ 3

    "{grid-area:span 3 / span somegridarea}"
      `shouldRenderFrom`
      gridArea $ span_ 3 // span_ "somegridarea"

    "{grid-area:inherit}"
      `shouldRenderFrom`
      gridArea (inherit :: GridLine)

    "{grid-area:initial}"
      `shouldRenderFrom`
      gridArea (initial :: GridLine)

    "{grid-area:unset}"
      `shouldRenderFrom`
      gridArea (unset :: GridLine)

  describe "grid area errors" $ do

    it "throw error when third value is 0" $ do
      gridArea (1 // 1 // 0)
        `shouldThrowErrorCall`
        "Value 0 is invalid"

    it "throw error when fourth value is 0" $ do
      gridArea (1 // 1 // 1 // 0)
        `shouldThrowErrorCall`
        "Value 0 is invalid"

    -- Must not compile as grid-area only accepts up to 4 arguments.
    -- gridArea $ 1 // 2 // 3 // 4 // 5

  describe "partialMkCustomIdentGrid errors" $ do

    it "throw error when value is span" $ do
      partialMkCustomIdentGrid "span"
        `shouldThrowCustomIdentError`
        "Custom-ident for a grid property cannot be named span"

    it "throw error when value is mempty" $ do
      partialMkCustomIdentGrid mempty
        `shouldThrowCustomIdentError`
        "Custom-ident cannot be empty"

    it "throw error when value starts with number" $ do
      partialMkCustomIdentGrid "1test"
        `shouldThrowCustomIdentError`
        "Custom-ident cannot start with a number"

    it "throw error when value starts with two hyphens" $ do
      partialMkCustomIdentGrid "--test"
        `shouldThrowCustomIdentError`
        "Custom-ident cannot start with two hyphens"

    it "throw error when value starts hyphen followed by number" $ do
      partialMkCustomIdentGrid "-2test"
        `shouldThrowCustomIdentError`
        "Custom-ident cannot start with a hyphen followed by a number"

shouldThrowCustomIdentError :: CustomIdentGrid -> String -> Expectation
shouldThrowCustomIdentError customIdent txt =
  evaluate (customIdentToText customIdent) `shouldThrow` errorCall txt
infixr 0 `shouldThrowCustomIdentError`
