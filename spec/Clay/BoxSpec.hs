{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Clay.BoxSpec where

import Control.Applicative
import Test.Hspec
import Clay
import Common

spec :: Spec
spec = do
  describe "box-shadow (Mozilla examples)" $ do
    describe "none" $ do
      "{box-shadow:none}" `shouldRenderAsFrom`
        "{-webkit-box-shadow:none;-moz-box-shadow:none;-ms-box-shadow:none;-o-box-shadow:none;box-shadow:none}" $
          boxShadow [none]

    describe "offset-x | offset-y | color" $ do
      "{box-shadow:60px -16px teal}" `shouldRenderAsFrom`
        "{-webkit-box-shadow:60px -16px #008080;-moz-box-shadow:60px -16px #008080;-ms-box-shadow:60px -16px #008080;-o-box-shadow:60px -16px #008080;box-shadow:60px -16px #008080}" $
          boxShadow . pure . bsColor teal $ shadow (px 60) (px (-16))

    describe "offset-x | offset-y | blur-radius | color" $ do
      "{box-shadow:10px 5px 5px black}" `shouldRenderAsFrom`
        "{-webkit-box-shadow:10px 5px 5px #000000;-moz-box-shadow:10px 5px 5px #000000;-ms-box-shadow:10px 5px 5px #000000;-o-box-shadow:10px 5px 5px #000000;box-shadow:10px 5px 5px #000000}" $
          boxShadow [bsColor black $ shadowWithBlur (px 10) (px 5) (px 5)]

    describe "offset-x | offset-y | blur-radius | spread-radius | color" $ do
      "{box-shadow:2px 2px 2px 1px rgba(0, 0, 0, 0.2)}" `shouldRenderAsFrom`
        "{-webkit-box-shadow:2px 2px 2px 1px rgba(0,0,0,0.2);-moz-box-shadow:2px 2px 2px 1px rgba(0,0,0,0.2);-ms-box-shadow:2px 2px 2px 1px rgba(0,0,0,0.2);-o-box-shadow:2px 2px 2px 1px rgba(0,0,0,0.2);box-shadow:2px 2px 2px 1px rgba(0,0,0,0.2)}" $
          boxShadow [rgba 0 0 0 0.2 `bsColor` shadowWithSpread (px 2) (px 2) (px 2) (px 1)]

    describe "inset | offset-x | offset-y | color" $ do
      "{box-shadow:inset 5em 1em gold}" `shouldRenderAsFrom`
        "{-webkit-box-shadow:inset 5em 1em #ffd700;-moz-box-shadow:inset 5em 1em #ffd700;-ms-box-shadow:inset 5em 1em #ffd700;-o-box-shadow:inset 5em 1em #ffd700;box-shadow:inset 5em 1em #ffd700}" $
          boxShadow [bsInset $ gold `bsColor` shadow (em 5) (em 1)]

    describe "Any number of shadows, separated by commas" $ do
      "{box-shadow:3px 3px red, -1em 0 0.4em olive}" `shouldRenderAsFrom`
        "{-webkit-box-shadow:3px 3px #ff0000,-1em 0 0.4em #808000;-moz-box-shadow:3px 3px #ff0000,-1em 0 0.4em #808000;-ms-box-shadow:3px 3px #ff0000,-1em 0 0.4em #808000;-o-box-shadow:3px 3px #ff0000,-1em 0 0.4em #808000;box-shadow:3px 3px #ff0000,-1em 0 0.4em #808000}" $
          boxShadow
          [ red `bsColor` shadow (px 3) (px 3)
          , olive `bsColor` shadowWithBlur (em (-1)) nil (em 0.4)
          ]

    describe "Global keywords" $ do
      "{box-shadow:inherit}" `shouldRenderAsFrom`
        "{-webkit-box-shadow:inherit;-moz-box-shadow:inherit;-ms-box-shadow:inherit;-o-box-shadow:inherit;box-shadow:inherit}" $
          boxShadow [inherit]
      "{box-shadow:initial}" `shouldRenderAsFrom`
        "{-webkit-box-shadow:initial;-moz-box-shadow:initial;-ms-box-shadow:initial;-o-box-shadow:initial;box-shadow:initial}" $
          boxShadow [initial]
      "{box-shadow:unset}" `shouldRenderAsFrom`
        "{-webkit-box-shadow:unset;-moz-box-shadow:unset;-ms-box-shadow:unset;-o-box-shadow:unset;box-shadow:unset}" $
          boxShadow [unset]
