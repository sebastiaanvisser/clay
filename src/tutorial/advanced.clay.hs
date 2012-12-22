{-# LANGUAGE OverloadedStrings #-}
import Clay
import Control.Monad
import Data.String

main = putCss $
  article ? forM_ [0..4] mySection

myShade :: Color
myShade = "#f80"

mySection :: Integer -> Css
mySection n =
  let idx = fromString (show n)
   in section # nthChild idx ?
      do backgroundColor (setA (n * 40) myShade)
         let w = 20 * fromIntegral n
         backgroundSize (px 20 `by` pct w)
         p ? if n > 2
               then textDecoration underline
               else color yellow
