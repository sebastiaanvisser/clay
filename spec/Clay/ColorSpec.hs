{-# LANGUAGE OverloadedStrings #-}
module Clay.ColorSpec where

import           Clay.Color
import           Control.Exception                 (evaluate)
import           Control.Exception.Base            (SomeException)
import           Test.Hspec

someException :: Selector SomeException
someException = const True

spec :: Spec
spec = do
    -- TODO Rgba and Hsla opacity values should be decimal fractions of 1.0
    describe "toRgba" $ do
        it "gives back the same Rgba" $
            toRgba (Rgba 255 255 255 255) `shouldBe` Rgba 255 255 255 255
        it "will not accept an Other" $
            evaluate (toRgba (Other "auto")) `shouldThrow` someException
        it "translates a whiteRgba Hsla" $
            toRgba (Hsla 360 1.0 1.0 255) `shouldBe` Rgba 255 255 255 255
        it "translates a blackRgba Hsla" $
            toRgba (Hsla 0 0.0 0.0 255) `shouldBe` Rgba 0 0 0 255
        it "translates a red Hsla" $
            toRgba (Hsla 0 1.0 0.5 255) `shouldBe` Rgba 255 0 0 255
        it "translates a green Hsla" $
            toRgba (Hsla 120 1.0 0.5 255) `shouldBe` Rgba 0 255 0 255
        it "translates a blue Hsla" $
            toRgba (Hsla 240 1.0 0.5 255) `shouldBe` Rgba 0 0 255 255
        it "translates a random Hsla color" $ do
            toRgba (Hsla 99 0.61 0.82 122) `shouldBe` Rgba 200 237 181 122
            toRgba (Hsla 154 0.79 0.46 255) `shouldBe` Rgba 24 209 129 255

    describe "toHsla" $ do
        it "gives back the same Hsla" $
            toHsla (Hsla 0 0.0 0.0 0) `shouldBe` Hsla 0 0.0 0.0 0
        it "will not accept an Other" $
            evaluate (toHsla (Other "auto")) `shouldThrow` someException
        it "translates a whiteRgba Rgba" $
            toHsla (Rgba 255 255 255 255) `shouldBe` Hsla 0 0.0 1.0 255
        it "translates a blackRgba Rgba" $
            toHsla (Rgba 0 0 0 255) `shouldBe` Hsla 0 0.0 0.0 255
        it "translates a red Rgba" $
            toHsla (Rgba 255 0 0 255) `shouldBe` Hsla 0 1.0 0.5 255
        it "translates a green Rgba" $
            toHsla (Rgba 0 255 0 255) `shouldBe` Hsla 120 1.0 0.5 255
        it "translates a blue Rgba" $
            toHsla (Rgba 0 0 255 255) `shouldBe` Hsla 240 1.0 0.5 255
        it "translates a random Rgba color" $ do
            toHsla (Rgba 201 237 181 122) `shouldBe` Hsla 98 0.609 0.82 122
            toHsla (Rgba 20 168 104 255) `shouldBe` Hsla 154 0.787 0.369 255

    describe "lerp" $ do
        let whiteRgba = Rgba 255 255 255 255
        let blackRgba = Rgba 0 0 0 255
        let halfRgba =  Rgba 127 127 127 255
        let transparentRgba = Rgba 255 255 255 0
        let transparentBlack = Rgba 0 0 0 0
        let halfTransparent = Rgba 127 127 127 127

        let whiteHsla = Hsla 0 0 1.0 255
        let blackHsla = Hsla 0 0 0 255
        let transparentHsla = Hsla 0 0 1.0 0
        let transparentBlackHsla = Hsla 0 0 0 0

        it "gives roughly the same HSL color back" $
            lerp 0.5 (Hsla 100 0.5 0.5 255) (Hsla 100 0.5 0.5 255)
                `shouldBe` Hsla 99 0.504 0.498 255
        it "gives the same RGB color back" $
            lerp 0.2 (Rgba 201 237 181 122) (Rgba 201 237 181 122)
                `shouldBe` Rgba 201 237 181 122
        it "gives the same color back at zero" $ do
            lerp 0 transparentRgba whiteRgba `shouldBe` transparentRgba
            lerp 0.001 transparentRgba whiteRgba `shouldBe` transparentRgba
            lerp (-0.001) transparentRgba whiteRgba `shouldBe` transparentRgba
            lerp 0 transparentHsla whiteHsla `shouldBe` transparentHsla
            lerp 0.001 transparentHsla whiteHsla `shouldBe` transparentHsla
            lerp (-0.001) transparentHsla whiteHsla `shouldBe` transparentHsla
        it "gives the same color back at negative 1 if the color is lower than the bound" $ do
            lerp (-1.0) transparentRgba whiteRgba `shouldBe` transparentRgba
            lerp (-1.0) transparentHsla whiteHsla `shouldBe` transparentHsla
        it "gives the bound color at 100%" $ do
            lerp 1.0 transparentRgba whiteRgba `shouldBe` whiteRgba
            lerp 1.001 transparentRgba whiteRgba `shouldBe` whiteRgba
            lerp 2.0 transparentRgba whiteRgba `shouldBe` whiteRgba
            lerp 0.999 transparentRgba whiteRgba `shouldBe` Rgba 255 255 255 254
            lerp 1.0 transparentHsla whiteHsla `shouldBe` whiteHsla
            lerp 1.001 transparentHsla whiteHsla `shouldBe` whiteHsla
            lerp 2.0 transparentHsla whiteHsla `shouldBe` whiteHsla
            lerp 0.999 transparentHsla whiteHsla `shouldBe` Hsla 0 0.0 1.0 254
        it "gives the right halfway color" $ do
            lerp 0.5 blackRgba whiteRgba `shouldBe` halfRgba
            lerp 0.5 transparentBlack whiteRgba `shouldBe` halfTransparent
            lerp 0.501 blackRgba whiteRgba `shouldBe` halfRgba
            lerp 0.499 blackRgba whiteRgba `shouldBe` halfRgba
            lerp 0.5 blackHsla whiteHsla `shouldBe` Hsla 0 0.0 0.498 255
            lerp 0.5 transparentBlackHsla whiteHsla `shouldBe` Hsla 0 0.0 0.498 127
            lerp 0.501 blackHsla whiteHsla `shouldBe` Hsla 0 0.0 0.498 255
            lerp 0.499 blackHsla whiteHsla `shouldBe` Hsla 0 0.0 0.498 255
        it "should lerp backwards" $ do
            lerp 0 whiteRgba blackRgba `shouldBe` whiteRgba
            lerp 1.0 whiteRgba blackRgba `shouldBe` blackRgba
            lerp 0.5 whiteRgba blackRgba `shouldBe` Rgba 128 128 128 255
            lerp 0 whiteHsla blackHsla `shouldBe` whiteHsla
            lerp 1.0 whiteHsla blackHsla `shouldBe` blackHsla
            lerp 0.5 whiteHsla blackHsla`shouldBe` Hsla 0 0.0 0.502 255
        it "should lerp down" $ do
            lerp (-1.0) blackRgba whiteRgba `shouldBe` blackRgba
            lerp (-0.5) blackRgba whiteRgba `shouldBe` blackRgba
            lerp (-0.5) halfRgba whiteRgba `shouldBe` Rgba 63 63 63 255
            lerp (-1.0) blackHsla whiteHsla  `shouldBe` blackHsla
            lerp (-0.5) blackHsla whiteHsla `shouldBe` blackHsla
            lerp (-0.5) (Hsla 0 0.0 0.5 127) whiteHsla `shouldBe` Hsla 0 0.0 0.247 63
        it "should lerp a red color" $ do
            lerp 0.5 (Rgba 255 0 0 255) whiteRgba `shouldBe` Rgba 255 127 127 255
            lerp (-0.5) (Rgba 255 0 0 255) whiteRgba `shouldBe` Rgba 255 0 0 255
            lerp 0.5 (Rgba 255 0 0 255) blackRgba `shouldBe` Rgba 128 0 0 255
            lerp (-0.5) (Rgba 255 0 0 255) blackRgba `shouldBe` Rgba 255 0 0 255

            lerp 0.5 (Hsla 0 1.0 0.5 255) whiteRgba `shouldBe` Hsla 0 1.0 0.749 255
            lerp 0.5 (Hsla 0 1.0 0.5 255) blackRgba `shouldBe` Hsla 0 1.0 0.251 255
        it "should lerp a green color" $ do
            lerp 0.5 (Rgba 0 255 0 255) whiteRgba `shouldBe` Rgba 127 255 127 255
            lerp (-0.5) (Rgba 0 255 0 255) whiteRgba `shouldBe` Rgba 0 255 0 255
            lerp 0.5 (Rgba 0 255 0 255) blackRgba `shouldBe` Rgba 0 128 0 255

            lerp 0.5 (Hsla 120 1.0 0.5 255) whiteRgba `shouldBe` Hsla 120 1.0 0.749 255
            lerp 0.5 (Hsla 120 1.0 0.5 255) blackRgba `shouldBe` Hsla 120 1.0 0.251 255
        it "should lerp a blue color" $ do
            lerp 0.5 (Rgba 0 0 255 255) whiteRgba `shouldBe` Rgba 127 127 255 255
            lerp (-0.5) (Rgba 0 0 255 255) whiteRgba `shouldBe` Rgba 0 0 255 255
            lerp 0.5 (Rgba 0 0 255 255) blackRgba `shouldBe` Rgba 0 0 128 255

            lerp 0.5 (Hsla 240 1.0 0.5 255) whiteRgba `shouldBe` Hsla 240 1.0 0.749 255
            lerp 0.5 (Hsla 240 1.0 0.5 255) blackRgba `shouldBe` Hsla 240 1.0 0.251 255
        it "should lerp a random color" $ do
            lerp 0.5 (Rgba 201 237 181 122) whiteRgba `shouldBe` Rgba 228 246 218 188
            lerp 0.5 (Rgba 201 237 181 122) blackRgba `shouldBe` Rgba 101 119 91 188

            lerp 0.5 (Hsla 99 0.61 0.82 122) whiteRgba `shouldBe` Hsla 100 0.609 0.91 188
            lerp 0.5 (Hsla 99 0.61 0.82 122) blackRgba `shouldBe` Hsla 100 0.133 0.412 188

    describe "lighten" $ do
        it "lightens an HSL color by a factor" $
            lighten 0.2 (Hsla 154 0.79 0.46 255) `shouldBe` Hsla 154 0.667 0.565 255
        it "lightens an RGB color by a factor" $
            lighten 0.2 (Rgba 25 210 130 255) `shouldBe` Rgba 71 219 155 255
        it "does not lighten an HSL color past white" $
            lighten 0.8 (Hsla 154 0.79 0.46 255) `shouldBe` Hsla 154 0.649 0.888 255
        it "does not lighten an RGB color past white" $
            lighten 0.8 (Rgba 25 210 130 255) `shouldBe` Rgba 209 246 230 255

    describe "darken" $ do
        it "darken an HSL color by a factor" $
            darken 0.2 (Hsla 154 0.79 0.46 255) `shouldBe` Hsla 154 0.787 0.369 255
        it "darken an RGB color by a factor" $
            darken 0.2 (Rgba 25 210 130 255) `shouldBe` Rgba 20 168 104 255
        it "does not darken an HSL color past black" $
            darken 0.8 (Hsla 154 0.79 0.46 255) `shouldBe` Hsla 154 0.787 0.092 255
        it "does not darken an RGB color past black" $
            darken 0.8 (Rgba 25 210 130 255) `shouldBe` Rgba 5 42 26 255
