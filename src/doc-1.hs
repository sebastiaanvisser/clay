{-# LANGUAGE OverloadedStrings #-}
import Clay; import Prelude hiding ((**), div)

myRule :: Css
myRule = html |> body ** div # ".container" ?
  do background  black
     color       white
     border      solid (px 1) green
