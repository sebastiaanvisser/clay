{-# LANGUAGE OverloadedStrings #-}
module Common where

import Clay

-- The color palette.

fstColor, sndColor, trdColor :: Color

fstColor = rgb 255 160 50
sndColor = rgb 56 135 190
trdColor = "#f8f8f8"

----------------------------------------------------------------------------

textFont, headerFont, codeFont, anchors :: Css

-- Font used for default text blocks.

textFont =
  do fontFamily     ["Europa", "Helvetica", sansSerif]
     textRendering  optimizeLegibility
     color "#222"

-- For now the header uses the default font.

headerFont = textFont

-- Font used for code blocks.

codeFont =
  do fontSize       (px 16)
     fontFamily     ["Monaco", "Courier New", monospace]
     lineHeight     (ex 2.6)
     textRendering  optimizeLegibility

-- Generic anchor styling.

anchors =
  a ? do textDecoration  none

         -- Subtle transition.
         transitions [ ("background-color" , sec 0.5, ease, sec 0)
                     , ("color"            , sec 0.2, ease, sec 0)
                     ]

         backgroundColor         (setA   0 yellow)
         hover & backgroundColor (setA  60 yellow)
         hover & color           black

----------------------------------------------------------------------------

-- Helper rule to horizontally center content divs.

centered :: Css
centered =
  do width        (px 800)
     boxSizing    borderBox
     sym2 margin  0 auto

