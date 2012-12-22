{-# LANGUAGE OverloadedStrings #-}
module Common where

import Clay

-- The color palette.

fstColor, sndColor, trdColor :: Color

fstColor = rgb 255 160 50
sndColor = rgb 56 135 190
trdColor = "#f8f8f8"

---------------------------------------------------------------------------

textFont, headerFont, codeFont :: Css

-- Font used for default text blocks.

textFont =
  do fontFamily     ["Europa", "Helvetica", sansSerif]
     textRendering  optimizeLegibility
     color "#222"
     a ? do textDecoration  none
            color           inherit

-- For now the header uses the default font.

headerFont = textFont

-- Font used for code blocks.

codeFont =
  do fontSize       (px 16)
     fontFamily     ["Monaco", "Courier New", monospace]
     lineHeight     (ex 2.6)
     textRendering  optimizeLegibility

