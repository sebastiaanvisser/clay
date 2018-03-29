{-# LANGUAGE OverloadedStrings #-}
module Clay.Color where

import Data.Char (isHexDigit)
import Data.Monoid
import Data.String
import Text.Printf

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Read as Text

import Clay.Property
import Clay.Common

-- * Color datatype.

data Color
  = Rgba Integer Integer Integer Float
  | Hsla Integer Float   Float   Float
  | Other Value
  deriving (Show, Eq)

-- * Color constructors.

rgba :: Integer -> Integer -> Integer -> Float -> Color
rgba = Rgba

rgb :: Integer -> Integer -> Integer -> Color
rgb r g b = rgba r g b 1

hsla :: Integer -> Float -> Float -> Float -> Color
hsla = Hsla

hsl :: Integer -> Float -> Float -> Color
hsl r g b = hsla r g b 1

grayish :: Integer -> Color
grayish g = rgb g g g

transparent :: Color
transparent = rgba 0 0 0 0

-- * Setting individual color components.

setR :: Integer -> Color -> Color
setR r (Rgba _ g b a) = Rgba r g b a
setR _ o              = o

setG :: Integer -> Color -> Color
setG g (Rgba r _ b a) = Rgba r g b a
setG _ o              = o

setB :: Integer -> Color -> Color
setB b (Rgba r g _ a) = Rgba r g b a
setB _ o              = o

setA :: Float -> Color -> Color
setA a (Rgba r g b _) = Rgba r g b a
setA a (Hsla r g b _) = Hsla r g b a
setA _ o              = o

-- * Color conversions.

toRgba :: Color -> Color
toRgba color =
    case color of
        Hsla h s l a -> toRgba' rgb' a
              where sextant = fromIntegral h / 60.0
                    chroma = (s *) . (1.0 -) . abs $ (2.0 * l) - 1.0
                    x = (chroma *) . (1.0 -) . abs $ (sextant `fracMod` 2) - 1.0
                    lightnessAdjustment = l - (chroma / 2.0)

                    toRgbPart component = truncate . (* 255.0) $ component + lightnessAdjustment
                    toRgba' (r, g, b) = Rgba (toRgbPart r) (toRgbPart g) (toRgbPart b)

                    rgb' | h >= 0   && h <  60 = (chroma, x     ,  0)
                         | h >= 60  && h < 120 = (x     , chroma,  0)
                         | h >= 120 && h < 180 = (0     , chroma,  x)
                         | h >= 180 && h < 240 = (0     , x     ,  chroma)
                         | h >= 240 && h < 300 = (x     , 0     ,  chroma)
                         | otherwise           = (chroma, 0     ,  x)

        c@(Rgba _ _ _ _) -> c
        
        Other _          -> error "Invalid to pass Other to toRgba."


toHsla :: Color -> Color
toHsla color =
    case color of
        Rgba redComponent greenComponent blueComponent alphaComponent -> Hsla h (decimalRound s 3) (decimalRound l 3) alphaComponent
            where r = fromIntegral redComponent   / 255.0
                  g = fromIntegral greenComponent / 255.0
                  b = fromIntegral blueComponent  / 255.0

                  minColor = minimum [r, g, b]
                  maxColor = maximum [r, g, b]
                  delta = maxColor - minColor

                  l = (minColor + maxColor) / 2.0
                  s = if delta == 0.0 then 0.0
                      else (delta /) . (1.0 -) . abs $ (2.0 * l) - 1.0

                  h' | delta == 0.0 = 0.0
                     | r == maxColor = ((g - b) / delta) `fracMod` 6.0
                     | g == maxColor = ((b - r) / delta) + 2.0
                     | otherwise     = ((r - g) / delta) + 4.0

                  h'' = truncate $ 60 * h'
                  h = if h'' < 0 then h''+ 360 else h''

        c@(Hsla _ _ _ _) -> c
        
        Other _          -> error "Invalid to pass Other to toHsla."

-- * Computing with colors.

(*.) :: Color -> Integer -> Color
(*.) (Rgba r g b a) i = Rgba (clamp (r * i)) (clamp (g * i)) (clamp (b * i)) a
(*.) o              _ = o

(+.) :: Color -> Integer -> Color
(+.) (Rgba r g b a) i = Rgba (clamp (r + i)) (clamp (g + i)) (clamp (b + i)) a
(+.) o              _ = o

(-.) :: Color -> Integer -> Color
(-.) (Rgba r g b a) i = Rgba (clamp (r - i)) (clamp (g - i)) (clamp (b - i)) a
(-.) o              _ = o

clamp :: Ord a => Num a => a -> a
clamp i = max (min i (fromIntegral (255 :: Integer))) (fromIntegral (0 :: Integer))

lighten :: Float -> Color -> Color
lighten factor color =
    case color of
        c@(Hsla {}) -> toHsla $ lighten factor (toRgba c)
        c@(Rgba {}) -> lerp factor c (Rgba 255 255 255 255)
        Other _     -> error "Other cannot be lightened."

darken :: Float -> Color -> Color
darken factor color =
    case color of
        c@(Hsla {}) -> toHsla $ darken factor (toRgba c)
        c@(Rgba {}) -> lerp factor c (Rgba 0 0 0 255)
        Other _     -> error "Other cannot be darkened."

lerp :: Float -> Color -> Color -> Color
lerp factor startColor boundColor =
    case (startColor, boundColor) of
        (Other _, _) -> error "Other cannot be lerped." 
        (_, Other _) -> error "Other cannot be lerped." 
        (color@(Hsla {}), bound) -> toHsla $ lerp factor (toRgba color) bound

        (start, color@(Hsla {})) -> toHsla $ lerp factor start (toRgba color)

        (Rgba r g b a, Rgba r' g' b' a') ->
            Rgba
                (lerpComponent factor r r')
                (lerpComponent factor g g')
                (lerpComponent factor b b')
                (lerpAlpha factor a a')
            where lerpComponent :: Float -> Integer -> Integer -> Integer
                  lerpComponent amount start bound =
                    let difference = bound - start
                        adjustment = truncate $ fromIntegral difference * amount
                    in clamp $ start + adjustment
                  lerpAlpha :: Float -> Float -> Float -> Float
                  lerpAlpha amount start bound =
                    let difference = bound - start
                        adjustment = fromIntegral $ (truncate $ difference * amount :: Integer)
                    in clamp $ start + adjustment

-------------------------------------------------------------------------------

instance Val Color where
  value clr =
    case clr of
      Rgba r g b 1.0 -> Value $mconcat ["#",  p' r, p' g, p' b]
      Rgba r g b a   -> Value $mconcat ["rgba(", p r, ",", p g, ",", p b, ",", ah a, ")"]
      Hsla h s l 1.0 -> Value $mconcat ["hsl(",  p h, ",", f s, ",", f l,            ")"]
      Hsla h s l a   -> Value $mconcat ["hsla(", p h, ",", f s, ",", f l, ",", ah a, ")"]
      Other o        -> o
    where p  = fromString . show
          p' = fromString . printf "%02x"
          f  = fromString . printf "%.4f%%"
          ah = fromString . take 6 . show

instance None    Color where none    = Other "none"
instance Auto    Color where auto    = Other "auto"
instance Inherit Color where inherit = Other "inherit"
instance Other   Color where other   = Other

instance IsString Color where
  fromString = parse . fromString

parse :: Text -> Color
parse t =
  case Text.uncons t of
    Just ('#', cs) | Text.all isHexDigit cs ->
      case Text.unpack cs of
        [a, b, c, d, e, f, g, h] -> rgba (hex a b) (hex c d) (hex e f) (fromIntegral (hex g h :: Integer) / 255.0)
        [a, b, c, d, e, f      ] -> rgb  (hex a b) (hex c d) (hex e f)
        [a, b, c, d            ] -> rgba (hex a a) (hex b b) (hex c c) (fromIntegral (hex d d :: Integer) / 255.0)
        [a, b, c               ] -> rgb  (hex a a) (hex b b) (hex c c)
        _                        -> err
    _                            -> err

  where
    hex a b = either err fst (Text.hexadecimal (Text.singleton a <> Text.singleton b))
    err = error "Invalid color string"

-------------------------------------------------------------------------------

-- * List of color values by name.

aliceblue, antiquewhite, aqua, aquamarine, azure, beige, bisque, black,
  blanchedalmond, blue, blueviolet, brown, burlywood, cadetblue, chartreuse,
  chocolate, coral, cornflowerblue, cornsilk, crimson, cyan, darkblue,
  darkcyan, darkgoldenrod, darkgray, darkgreen, darkgrey, darkkhaki,
  darkmagenta, darkolivegreen, darkorange, darkorchid, darkred, darksalmon,
  darkseagreen, darkslateblue, darkslategray, darkslategrey, darkturquoise,
  darkviolet, deeppink, deepskyblue, dimgray, dimgrey, dodgerblue, firebrick,
  floralwhite, forestgreen, fuchsia, gainsboro, ghostwhite, gold, goldenrod,
  gray, green, greenyellow, grey, honeydew, hotpink, indianred, indigo, ivory,
  khaki, lavender, lavenderblush, lawngreen, lemonchiffon, lightblue,
  lightcoral, lightcyan, lightgoldenrodyellow, lightgray, lightgreen,
  lightgrey, lightpink, lightsalmon, lightseagreen, lightskyblue,
  lightslategray, lightslategrey, lightsteelblue, lightyellow, lime, limegreen,
  linen, magenta, maroon, mediumaquamarine, mediumblue, mediumorchid,
  mediumpurple, mediumseagreen, mediumslateblue, mediumspringgreen,
  mediumturquoise, mediumvioletred, midnightblue, mintcream, mistyrose,
  moccasin, navajowhite, navy, oldlace, olive, olivedrab, orange, orangered,
  orchid, palegoldenrod, palegreen, paleturquoise, palevioletred, papayawhip,
  peachpuff, peru, pink, plum, powderblue, purple, red, rosybrown, royalblue,
  saddlebrown, salmon, sandybrown, seagreen, seashell, sienna, silver, skyblue,
  slateblue, slategray, slategrey, snow, springgreen, steelblue, tan, teal,
  thistle, tomato, turquoise, violet, wheat, white, whitesmoke, yellow,
  yellowgreen :: Color

aliceblue            = rgb 240 248 255
antiquewhite         = rgb 250 235 215
aqua                 = rgb   0 255 255
aquamarine           = rgb 127 255 212
azure                = rgb 240 255 255
beige                = rgb 245 245 220
bisque               = rgb 255 228 196
black                = rgb   0   0   0
blanchedalmond       = rgb 255 235 205
blue                 = rgb   0   0 255
blueviolet           = rgb 138  43 226
brown                = rgb 165  42  42
burlywood            = rgb 222 184 135
cadetblue            = rgb  95 158 160
chartreuse           = rgb 127 255   0
chocolate            = rgb 210 105  30
coral                = rgb 255 127  80
cornflowerblue       = rgb 100 149 237
cornsilk             = rgb 255 248 220
crimson              = rgb 220  20  60
cyan                 = rgb   0 255 255
darkblue             = rgb   0   0 139
darkcyan             = rgb   0 139 139
darkgoldenrod        = rgb 184 134  11
darkgray             = rgb 169 169 169
darkgreen            = rgb   0 100   0
darkgrey             = rgb 169 169 169
darkkhaki            = rgb 189 183 107
darkmagenta          = rgb 139   0 139
darkolivegreen       = rgb  85 107  47
darkorange           = rgb 255 140   0
darkorchid           = rgb 153  50 204
darkred              = rgb 139   0   0
darksalmon           = rgb 233 150 122
darkseagreen         = rgb 143 188 143
darkslateblue        = rgb  72  61 139
darkslategray        = rgb  47  79  79
darkslategrey        = rgb  47  79  79
darkturquoise        = rgb   0 206 209
darkviolet           = rgb 148   0 211
deeppink             = rgb 255  20 147
deepskyblue          = rgb   0 191 255
dimgray              = rgb 105 105 105
dimgrey              = rgb 105 105 105
dodgerblue           = rgb  30 144 255
firebrick            = rgb 178  34  34
floralwhite          = rgb 255 250 240
forestgreen          = rgb 34  139  34
fuchsia              = rgb 255   0 255
gainsboro            = rgb 220 220 220
ghostwhite           = rgb 248 248 255
gold                 = rgb 255 215   0
goldenrod            = rgb 218 165  32
gray                 = rgb 128 128 128
green                = rgb   0 128   0
greenyellow          = rgb 173 255  47
grey                 = rgb 128 128 128
honeydew             = rgb 240 255 240
hotpink              = rgb 255 105 180
indianred            = rgb 205  92  92
indigo               = rgb 75    0 130
ivory                = rgb 255 255 240
khaki                = rgb 240 230 140
lavender             = rgb 230 230 250
lavenderblush        = rgb 255 240 245
lawngreen            = rgb 124 252   0
lemonchiffon         = rgb 255 250 205
lightblue            = rgb 173 216 230
lightcoral           = rgb 240 128 128
lightcyan            = rgb 224 255 255
lightgoldenrodyellow = rgb 250 250 210
lightgray            = rgb 211 211 211
lightgreen           = rgb 144 238 144
lightgrey            = rgb 211 211 211
lightpink            = rgb 255 182 193
lightsalmon          = rgb 255 160 122
lightseagreen        = rgb  32 178 170
lightskyblue         = rgb 135 206 250
lightslategray       = rgb 119 136 153
lightslategrey       = rgb 119 136 153
lightsteelblue       = rgb 176 196 222
lightyellow          = rgb 255 255 224
lime                 = rgb   0 255   0
limegreen            = rgb  50 205  50
linen                = rgb 250 240 230
magenta              = rgb 255   0 255
maroon               = rgb 128   0   0
mediumaquamarine     = rgb 102 205 170
mediumblue           = rgb   0   0 205
mediumorchid         = rgb 186  85 211
mediumpurple         = rgb 147 112 219
mediumseagreen       = rgb  60 179 113
mediumslateblue      = rgb 123 104 238
mediumspringgreen    = rgb   0 250 154
mediumturquoise      = rgb  72 209 204
mediumvioletred      = rgb 199  21 133
midnightblue         = rgb  25  25 112
mintcream            = rgb 245 255 250
mistyrose            = rgb 255 228 225
moccasin             = rgb 255 228 181
navajowhite          = rgb 255 222 173
navy                 = rgb   0   0 128
oldlace              = rgb 253 245 230
olive                = rgb 128 128   0
olivedrab            = rgb 107 142  35
orange               = rgb 255 165   0
orangered            = rgb 255 69    0
orchid               = rgb 218 112 214
palegoldenrod        = rgb 238 232 170
palegreen            = rgb 152 251 152
paleturquoise        = rgb 175 238 238
palevioletred        = rgb 219 112 147
papayawhip           = rgb 255 239 213
peachpuff            = rgb 255 218 185
peru                 = rgb 205 133  63
pink                 = rgb 255 192 203
plum                 = rgb 221 160 221
powderblue           = rgb 176 224 230
purple               = rgb 128   0 128
red                  = rgb 255   0   0
rosybrown            = rgb 188 143 143
royalblue            = rgb  65 105 225
saddlebrown          = rgb 139  69  19
salmon               = rgb 250 128 114
sandybrown           = rgb 244 164  96
seagreen             = rgb  46 139  87
seashell             = rgb 255 245 238
sienna               = rgb 160  82  45
silver               = rgb 192 192 192
skyblue              = rgb 135 206 235
slateblue            = rgb 106  90 205
slategray            = rgb 112 128 144
slategrey            = rgb 112 128 144
snow                 = rgb 255 250 250
springgreen          = rgb   0 255 127
steelblue            = rgb  70 130 180
tan                  = rgb 210 180 140
teal                 = rgb   0 128 128
thistle              = rgb 216 191 216
tomato               = rgb 255  99  71
turquoise            = rgb  64 224 208
violet               = rgb 238 130 238
wheat                = rgb 245 222 179
white                = rgb 255 255 255
whitesmoke           = rgb 245 245 245
yellow               = rgb 255 255   0
yellowgreen          = rgb 154 205  50

