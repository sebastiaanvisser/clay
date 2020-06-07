{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Clay.SelectorSpec where

import Clay
import Common
import Test.Hspec

spec :: Spec
spec = describe "Selector with filtering" $
    describe "[x # (y ^= z)]" $
      ".row[class^=\"col\"]" `shouldRenderAsFrom`
        ".row [class^='col']{color:#ffffff}" $
          ".row" # ("class" ^= "col") ? color white
