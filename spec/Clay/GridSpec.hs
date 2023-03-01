{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Clay.GridSpec where

import Test.Hspec
import Common
import Clay

spec :: Spec
spec = do
  describe "grid row start" $ do
    "{grid-row-start:3}"
      `shouldRenderFrom`
      gridRowStart 3

    "{grid-row-start:-2}"
      `shouldRenderFrom`
      gridRowStart (-2)

    "{grid-row-start:span nav}"
      `shouldRenderFrom`
      gridRowStart $ span_ "nav"

    "{grid-row-start:footer 4}"
      `shouldRenderFrom`
      gridRowStart ("footer", 4)

  describe "grid row" $ do

    "{grid-row:nav}"
      `shouldRenderFrom`
      gridRow "nav"

    "{grid-row:3 / span 2}"
      `shouldRenderFrom`
      gridRow $ 3 // span_ 2

    "{grid-row:nav / 2}"
      `shouldRenderFrom`
      gridRow $ "nav" // 2

    -- Must not compile as grid-row only accepts up to 2 arguments.
    -- gridRow $ "nav" // 2 // 3

  describe "grid area" $ do
    "{grid-area:3}"
      `shouldRenderFrom`
      gridArea 3

    "{grid-area:2 / nav}"
      `shouldRenderFrom`
      gridArea $ 2 // "nav"

    "{grid-area:nav 2 / span 3 / 4}"
      `shouldRenderFrom`
      gridArea $ ("nav", 2) // span_ 3 // 4

    "{grid-area:nav / span nav 3 / 4 / nav 3}"
      `shouldRenderFrom`
      gridArea $ "nav" // span_ ("nav", 3) // 4 // ("nav", 3)

    -- Must not compile as grid-area only accepts up to 4 arguments.
    -- gridArea $ 1 // 2 // 3 // 4 // 5
