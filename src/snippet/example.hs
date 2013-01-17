{-# LANGUAGE
    OverloadedStrings
  #-}
import Clay

menu :: Css
menu = header |> nav ?
  do background    white
     color         "#04a"
     fontSize      (px 24)
     padding       20 0 20 0
     textTransform uppercase
     position      absolute
     left          0
     right         0
     bottom        (px (-72))
