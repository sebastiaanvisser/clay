import Data.Monoid
import Prelude hiding ((**))
import Clay

main = putCss $
  (body <> article) ** p |> (a <> b) # lastOfType ?
      color red
