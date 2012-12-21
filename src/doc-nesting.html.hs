import Clay

main = putCss $
  do p ? color red
     b ? color yellow
     article ?
       do strong ? background black
          abbr  <? fontVariant smallCaps
