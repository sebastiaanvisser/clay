import Data.Monoid
import Prelude hiding ((**))
import Clay

main = putCss $
  (body <> abbr) ** p |> (a <> b) # lastOfType ?
      color red
