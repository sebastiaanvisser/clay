import Clay

main = putCss $
  body ?
    do let rot = deg 30
       transform (rotateX rot)
       background $
         linearGradient (angular rot)
           [ ( red    ,   0 )
           , ( yellow ,  40 )
           , ( blue   , 100 )
           ]
