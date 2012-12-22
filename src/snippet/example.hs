{-# LANGUAGE OverloadedStrings #-}
import Clay

menu :: Css
menu = header |> nav ?
  do background     white
     boxShadow      0 0 (px 60)
                    "#00000018"
     fontSize       (px 24)
     sym2 padding   20 0
     textTransform  uppercase
     position       absolute
     left           0
     right          0
     bottom         (px (-72))
