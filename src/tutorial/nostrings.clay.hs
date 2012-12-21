import Data.Monoid
import Clay

main = putCss $
  a # href # hover ? textDecoration none
