{-# LANGUAGE OverloadedStrings #-}
module Clay.TextSpec where

import Test.Hspec
import Clay
import Common

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

  describe "hyphens" $ do
    "{hyphens:none}" `shouldRenderFrom`
      hyphens none
    "{hyphens:manual}" `shouldRenderFrom`
      hyphens manual
    "{hyphens:auto}" `shouldRenderFrom`
      hyphens auto
    "{hyphens:initial}" `shouldRenderFrom`
      hyphens initial
    "{hyphens:inherit}" `shouldRenderFrom`
      hyphens inherit
    "{hyphens:unset}" `shouldRenderFrom`
      hyphens unset
    "{hyphens:nonsense}" `shouldRenderFrom`
      hyphens (other "nonsense")

  describe "hyphenate-character" $ do
    "{hyphenate-character:\"hyphen\"}" `shouldRenderFrom`
      hyphenateCharacter "hyphen"
    "{hyphenate-character:\"\\\"quoted\\\"\"}" `shouldRenderFrom`
      hyphenateCharacter "\"quoted\""
    "{hyphenate-character:auto}" `shouldRenderFrom`
      hyphenateCharacter auto
    "{hyphenate-character:initial}" `shouldRenderFrom`
      hyphenateCharacter initial
    "{hyphenate-character:inherit}" `shouldRenderFrom`
      hyphenateCharacter inherit
    "{hyphenate-character:unset}" `shouldRenderFrom`
      hyphenateCharacter unset
    "{hyphenate-character:nonsense}" `shouldRenderFrom`
      hyphenateCharacter (other "nonsense")

  describe "hyphenate-limit-chars" $ do
    "{hyphenate-limit-chars:10 11 12}" `shouldRenderFrom`
      hyphenateLimitChars 10 11 12
    "{hyphenate-limit-chars:auto 11 unset}" `shouldRenderFrom`
      hyphenateLimitChars auto 11 unset
    "{hyphenate-limit-chars:nonsense initial inherit}" `shouldRenderFrom`
      hyphenateLimitChars (other "nonsense") initial inherit
