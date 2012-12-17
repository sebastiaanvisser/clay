navigation :: Css
navigation =
  do fontSize (px 24)
     sym2 padding 20 0
     textTransform uppercase

     div <? centered
     a ? do paddingRight (px 43)
            color highlight
            lastOfType & paddingRight (px 0)
            hover      & color black

centered :: Css
centered =
  do width (px 800)
     boxSizing borderBox
     sym2 margin 0 auto

