{-# LANGUAGE OverloadedStrings #-}

module Clay.MediaSpec (spec) where

import Clay (compact, Css, em, opacity, query, renderWith)
import Clay.Media
import Clay.Render (Config(..))
import Clay.Stylesheet (Feature, keyframes, MediaType)
import Common
import Data.Text.Lazy as Text (Text, toStrict, unlines, unpack)
import Test.Hspec
import Prelude hiding (all, print)

spec :: Spec
spec = do
  describe "media types" $ do
    "all" `shouldRenderFromMediaType` all
    "screen" `shouldRenderFromMediaType` screen
    "print" `shouldRenderFromMediaType` print

    context "with deprecated types" $ do
      "aural" `shouldRenderFromMediaType` aural
      "braille" `shouldRenderFromMediaType` braille
      "handheld" `shouldRenderFromMediaType` handheld
      "projection" `shouldRenderFromMediaType` projection
      "tty" `shouldRenderFromMediaType` tty
      "tv" `shouldRenderFromMediaType` tv
      "embossed" `shouldRenderFromMediaType` embossed

  describe "geometrical features" $ do
    "width: 11em" `shouldRenderFromFeature` width (em 11)
    "min-width: 12em" `shouldRenderFromFeature` minWidth (em 12)
    "max-width: 13em" `shouldRenderFromFeature` maxWidth (em 13)
    "height: 14em" `shouldRenderFromFeature` height (em 14)
    "min-height: 15em" `shouldRenderFromFeature` minHeight (em 15)
    "max-height: 16em" `shouldRenderFromFeature` maxHeight (em 16)
    "device-width: 17em" `shouldRenderFromFeature` deviceWidth (em 17)
    "min-device-width: 18em" `shouldRenderFromFeature` minDeviceWidth (em 18)
    "max-device-width: 19em" `shouldRenderFromFeature` maxDeviceWidth (em 19)
    "device-height: 20em" `shouldRenderFromFeature` deviceHeight (em 20)
    "min-device-height: 21em" `shouldRenderFromFeature` minDeviceHeight (em 21)
    "max-device-height: 22em" `shouldRenderFromFeature` maxDeviceHeight (em 22)

  describe "aspect ratio features" $ do
    "aspect-ratio: 2/3" `shouldRenderFromFeature` aspectRatio (2, 3)
    "min-aspect-ratio: 4/5" `shouldRenderFromFeature` minAspectRatio (4, 5)
    "max-aspect-ratio: 6/7" `shouldRenderFromFeature` maxAspectRatio (6, 7)
    "device-aspect-ratio: 2/3" `shouldRenderFromFeature` deviceAspectRatio (2, 3)
    "min-device-aspect-ratio: 4/5" `shouldRenderFromFeature` minDeviceAspectRatio (4, 5)
    "max-device-aspect-ratio: 6/7" `shouldRenderFromFeature` maxDeviceAspectRatio (6, 7)

  describe "color features" $ do
    "color" `shouldRenderFromFeature` color
    "monochrome" `shouldRenderFromFeature` monochrome
    "scan" `shouldRenderFromFeature` scan
    "grid" `shouldRenderFromFeature` grid
    "min-color: 23" `shouldRenderFromFeature` minColor 23
    "max-color: 25" `shouldRenderFromFeature` maxColor 25
    "color-index: 15" `shouldRenderFromFeature` colorIndex 15
    "min-color-index: 17" `shouldRenderFromFeature` minColorIndex 17
    "max-color-index: 19" `shouldRenderFromFeature` maxColorIndex 19
    "min-monochrome: 77" `shouldRenderFromFeature` minMonochrome 77
    "max-monochrome: 99" `shouldRenderFromFeature` maxMonochrome 99

  describe "resolution features" $ do
    "resolution: 45dpi" `shouldRenderFromFeature` resolution (dpi 45)
    "resolution: 74dppx" `shouldRenderFromFeature` resolution (dppx 74)
    "min-resolution: 45dpi" `shouldRenderFromFeature` minResolution (dpi 45)
    "min-resolution: 74dppx" `shouldRenderFromFeature` minResolution (dppx 74)
    "max-resolution: 45dpi" `shouldRenderFromFeature` maxResolution (dpi 45)
    "max-resolution: 74dppx" `shouldRenderFromFeature` maxResolution (dppx 74)

  describe "preference features" $ do
    "prefers-color-scheme: light" `shouldRenderFromFeature` prefersColorScheme light
    "prefers-color-scheme: dark" `shouldRenderFromFeature` prefersColorScheme dark

  describe "keyframes tests" $ do
    it "keyframes test 1" $
      (renderWith compact [] $ keyframes "blink-animation" [(0, opacity 0), (100, opacity 1)])
        `shouldBe` ("@-webkit-keyframes blink-animation{0% {opacity:0}100% {opacity:1}}" <>
                    "@-moz-keyframes blink-animation{0% {opacity:0}100% {opacity:1}}" <>
                    "@-ms-keyframes blink-animation{0% {opacity:0}100% {opacity:1}}" <>
                    "@-o-keyframes blink-animation{0% {opacity:0}100% {opacity:1}}" <>
                    "@keyframes blink-animation{0% {opacity:0}100% {opacity:1}}")

-- | Empty CSS for when CSS is needed but we don't care about it.
emptyStyle :: Css
emptyStyle = pure ()

-- | The text should be rendered from the media type.
shouldRenderFromMediaType :: Text -> MediaType -> SpecWith ()
shouldRenderFromMediaType text mediaType =
  shouldRenderAsFrom (unpack text) fullText css
  where
    fullText = "@media " <> text <> "{}"
    css = query mediaType [] emptyStyle

-- | The text should be rendered from the feature.
shouldRenderFromFeature :: Text -> Feature -> SpecWith ()
shouldRenderFromFeature text feature =
  shouldRenderAsFrom (unpack text) fullText css
  where
    fullText = "@media all and (" <> text <> "){}"
    css = query all [feature] emptyStyle

infixr 0 `shouldRenderFromMediaType`

infixr 0 `shouldRenderFromFeature`
