import Clay
import qualified Data.Text.Lazy.IO as Text

main :: IO ()
main = Text.putStr (render myStylesheet)

myStylesheet :: Css
myStylesheet = body ? background red
