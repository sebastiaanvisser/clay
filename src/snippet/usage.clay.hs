import Clay

main :: IO ()
main = putCss myStylesheet

myStylesheet :: Css
myStylesheet = body ? background red
