{-# LANGUAGE OverloadedStrings #-}
import Clay

main = putCss $
  section # "#open" |> ".link" ?
    do textDecoration none
       "@href" &
         do textDecoration underline
            ":hover" & fontWeight bold
