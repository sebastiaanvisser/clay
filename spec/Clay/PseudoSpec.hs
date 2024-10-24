{-# LANGUAGE OverloadedStrings, OverloadedLists, TypeApplications #-}
module Clay.PseudoSpec where

import Test.Hspec
import Clay
import Common
import Prelude hiding (not, div)

spec :: Spec
spec = do
  describe "not pseudo selector" $ do
    describe "applied to a selector" $ do
      ":not(p){display:none}" `shouldRenderFrom` not p & display none
    describe "applied to a refinement" $ do
      "input:not(:checked){display:none}" `shouldRenderFrom` input # not checked ? display none
    describe "applied to both a selector and a refinement" $ do
      ":not(#some-input:not(:checked)){display:none}" `shouldRenderFrom` not ("#some-input" # not checked) & display none
    describe "applied to a overloaded string" $ do
      "#some-input:not(:checked) ~ #some-div > div:not(.test){display:none}" `shouldRenderFrom` "#some-input" # not checked |~ "#some-div" |> div # not @Clay.Selector ".test" ? display none
