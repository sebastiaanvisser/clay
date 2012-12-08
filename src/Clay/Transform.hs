{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Clay.Transform where

import Data.Monoid
import Prelude hiding (Left, Right)

import Clay.Property
import Clay.Rule
import Clay.Size
import Clay.Common

newtype Transform = Transform Value
  deriving (Val, None)

transform :: Transform -> Css
transform = prefixed (browsers <> "transform")

transforms :: [Transform] -> Css
transforms xs = prefixed (browsers <> "transform") (noCommas xs)

-------------------------------------------------------------------------------

scale :: Double -> Double -> Transform
scale x y = Transform ("scale(" <> value [x, y] <> ")")

scaleX :: Double -> Transform
scaleX x = Transform ("scaleX(" <> value x <> ")")

scaleY :: Double -> Transform
scaleY y = Transform ("scaleY(" <> value y <> ")")

scaleZ :: Double -> Transform
scaleZ z = Transform ("scaleZ(" <> value z <> ")")

scale3d :: Double -> Double -> Double -> Transform
scale3d x y z = Transform ("scale3d(" <> value [x, y, z] <> ")")

-------------------------------------------------------------------------------

rotate :: Angle a -> Angle a -> Transform
rotate x y = Transform ("rotate(" <> value [x, y] <> ")")

rotateX :: Angle a -> Transform
rotateX x = Transform ("rotateX(" <> value x <> ")")

rotateY :: Angle a -> Transform
rotateY y = Transform ("rotateY(" <> value y <> ")")

rotateZ :: Angle a -> Transform
rotateZ z = Transform ("rotateZ(" <> value z <> ")")

rotate3d :: Double -> Double -> Double -> Angle a -> Transform
rotate3d x y z a = Transform ("rotate3d(" <> value [value x, value y, value z, value a] <> ")")

-------------------------------------------------------------------------------

translate :: Size Abs -> Size Abs -> Transform
translate x y = Transform ("translate(" <> value [x, y] <> ")")

translateX :: Size Abs -> Transform
translateX x = Transform ("translateX(" <> value x <> ")")

translateY :: Size Abs -> Transform
translateY y = Transform ("translateY(" <> value y <> ")")

translateZ :: Size Abs -> Transform
translateZ z = Transform ("translateZ(" <> value z <> ")")

translate3d :: Size Abs -> Size Abs -> Size Abs -> Transform
translate3d x y z = Transform ("translate3d(" <> value [x, y, z] <> ")")

-------------------------------------------------------------------------------

skew :: Angle a -> Angle a -> Transform
skew x y = Transform ("skew(" <> value [x, y] <> ")")

skewX :: Angle a -> Transform
skewX x = Transform ("skewX(" <> value x <> ")")

skewY :: Angle a -> Transform
skewY y = Transform ("skewY(" <> value y <> ")")

-------------------------------------------------------------------------------

perspective :: Double -> Transform
perspective p = Transform ("perspective(" <> value p <> ")")

matrix :: Double -> Double -> Double -> Double -> Double -> Double -> Transform
matrix u v w x y z = Transform ("matrix3d(" <> value [ u, v, w, x, y, z ] <> ")")

matrix3d :: Double -> Double -> Double -> Double
         -> Double -> Double -> Double -> Double
         -> Double -> Double -> Double -> Double
         -> Double -> Double -> Double -> Double
         -> Transform
matrix3d w0 x0 y0 z0
         w1 x1 y1 z1
         w2 x2 y2 z2
         w3 x3 y3 z3 =
  Transform ("matrix3d(" <> value
       [ w0, x0, y0, z0
       , w1, x1, y1, z1
       , w2, x2, y2, z2
       , w3, x3, y3, z3
       ] <> ")")

