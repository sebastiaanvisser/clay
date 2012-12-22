{-# LANGUAGE OverloadedStrings #-}
module Header where

import Clay hiding (i, s, id)
import Data.Monoid
import Prelude hiding (div, span)

import Common

----------------------------------------------------------------------------

-- Styling for the header section.

theHeader :: Css
theHeader =
  do background  $ vGradient
                   (fstColor -. 80)
                   (fstColor +. 20)
     position    fixed
     top         (px 0)
     left        (px 0)
     right       (px 0)
     height      (px 288)

     before &
       do position        absolute
          top             (px 0)
          bottom          (px 0)
          left            (px 0)
          right           (px 0)
          content         (stringContent "")
          pointerEvents   none
          backgroundSize  (pct 100 `by` px 5)
          backgroundImage $ repeatingLinearGradient (straight sideTop)
                            [ ( setA  0 white,   0)
                            , ( setA 20 white,  50)
                            , ( setA  0 white, 100)
                            ]

     nav      ? theMenu
     "#logo" <? theLogo

theMenu :: Css
theMenu =

  do background     (setA 249 white)
     boxShadow      0 0 (px 60) (setA 20 black)
     fontSize       (px 24)
     sym2 padding   20 0
     textTransform  uppercase
     position       absolute
     left           0
     right          0
     bottom         (px (-72))


     div <? centered

     a ? do paddingRight    (px 45)
            transition      "color" (sec 0.4) ease (sec 0)
            textDecoration  none
            color           sndColor

            lastOfType & paddingRight (px 0)
            hover & color black

----------------------------------------------------------------------------

-- The site logo.

theLogo :: Css
theLogo =

  do centered
     paddingTop     (px 60)
     paddingBottom  (px 60)
     height         (pct 100)
     overflow       hidden

     backgroundImage $
       radialGradient sideCenter ellipse
         [ ( setA 150 yellow ,  0 )
         , ( setA  25 yellow , 50 )
         , ( setA   0 yellow , 65 )
         ]

     a ? do textDecoration none
            color inherit

     h1 <> h2 ?
       do textTransform  uppercase
          textAlign      (alignSide sideCenter)
          sym margin     0

     h1 ?
       do fontSize    (px 90)
          color       (setA 200 white)
          textShadow  0 0 (px 20)
                      (setA 200 (fstColor -. 80))

          -- Some custom kerning.
          letterSpacing                (em 0.40)
          span # ".a" ? letterSpacing  (em 0.36)
          span # ".y" ? letterSpacing  (em 0.00)

     h2 ?
       do fontSize       (px 35)
          color          (setA 120 black)
          letterSpacing  (em 0.3)
          a # hover ?
            color (setA 220 black)

