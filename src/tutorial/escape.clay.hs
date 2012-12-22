{-# LANGUAGE OverloadedStrings #-}
import Clay

main = putCss $
  star ?
    do color (other "ultraviolet")
       "-ms-lens-flare-style" -: "really-shiny"
