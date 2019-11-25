{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Clay.Transform
(

-- * The transform propery.

  Transformation
, transform
, transforms

-- * Translating.

, translate
, translateX, translateY, translateZ
, translate3d

-- * Scaling.

, scale
, scaleX, scaleY, scaleZ
, scale3d

-- * Rotating.

, rotate
, rotateX, rotateY, rotateZ
, rotate3d

-- * Skewing.

, skew
, skewX, skewY

-- * Custom 3D transformations.

, perspective
, matrix
, matrix3d 
)
where

import Prelude hiding (Left, Right)

import Clay.Property
import Clay.Stylesheet
import Clay.Size
import Clay.Common

newtype Transformation = Transformation Value
  deriving (Val, None)

transform :: Transformation -> Css
transform = prefixed (browsers <> "transform")

transforms :: [Transformation] -> Css
transforms xs = prefixed (browsers <> "transform") (noCommas xs)

-------------------------------------------------------------------------------

scale :: Double -> Double -> Transformation
scale x y = Transformation ("scale(" <> value [x, y] <> ")")

scaleX, scaleY, scaleZ :: Double -> Transformation

scaleX x = Transformation ("scaleX(" <> value x <> ")")
scaleY y = Transformation ("scaleY(" <> value y <> ")")
scaleZ z = Transformation ("scaleZ(" <> value z <> ")")

scale3d :: Double -> Double -> Double -> Transformation
scale3d x y z = Transformation ("scale3d(" <> value [x, y, z] <> ")")

-------------------------------------------------------------------------------

rotate :: Angle a -> Transformation
rotate x = Transformation ("rotate(" <> value x <> ")")

rotateX, rotateY, rotateZ :: Angle a -> Transformation

rotateX x = Transformation ("rotateX(" <> value x <> ")")
rotateY y = Transformation ("rotateY(" <> value y <> ")")
rotateZ z = Transformation ("rotateZ(" <> value z <> ")")

rotate3d :: Double -> Double -> Double -> Angle a -> Transformation
rotate3d x y z a = Transformation ("rotate3d(" <> value [value x, value y, value z, value a] <> ")")

-------------------------------------------------------------------------------

translate :: Size a -> Size b -> Transformation
translate x y = Transformation ("translate(" <> value [value x, value y] <> ")")

translateX, translateY :: Size LengthUnit -> Transformation
translateZ :: Size LengthUnit -> Transformation

translateX x = Transformation ("translateX(" <> value x <> ")")
translateY y = Transformation ("translateY(" <> value y <> ")")
translateZ z = Transformation ("translateZ(" <> value z <> ")")

translate3d :: Size a -> Size b -> Size LengthUnit -> Transformation
translate3d x y z = Transformation ("translate3d(" <> value [value x, value y, value z] <> ")")

-------------------------------------------------------------------------------

skew :: Angle a -> Angle a -> Transformation
skew x y = Transformation ("skew(" <> value [x, y] <> ")")

skewX, skewY :: Angle a -> Transformation

skewX x = Transformation ("skewX(" <> value x <> ")")
skewY y = Transformation ("skewY(" <> value y <> ")")

-------------------------------------------------------------------------------

perspective :: Double -> Transformation
perspective p = Transformation ("perspective(" <> value p <> ")")

matrix :: Double -> Double -> Double -> Double -> Double -> Double -> Transformation
matrix u v w x y z = Transformation ("matrix3d(" <> value [ u, v, w, x, y, z ] <> ")")

matrix3d :: Double -> Double -> Double -> Double
         -> Double -> Double -> Double -> Double
         -> Double -> Double -> Double -> Double
         -> Double -> Double -> Double -> Double
         -> Transformation
matrix3d w0 x0 y0 z0
         w1 x1 y1 z1
         w2 x2 y2 z2
         w3 x3 y3 z3 =
  Transformation ("matrix3d(" <> value
       [ w0, x0, y0, z0
       , w1, x1, y1, z1
       , w2, x2, y2, z2
       , w3, x3, y3, z3
       ] <> ")")

