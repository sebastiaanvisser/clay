{-# LANGUAGE
    OverloadedStrings
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  #-}
module Clay.Mask
(
-- * Generic mask property.

  Mask (mask)

-- * The mask-composite.

, MaskComposite
, clear, copy
, sourceOver, sourceIn, sourceOut, sourceAtop
, destinationOver, destinationIn, destinationOut, destinationAtop
, xor
, maskComposite
, maskComposites

-- * The mask-position.

, maskPosition
, maskPositions

-- * The mask-size.

, maskSize
, maskSizes

-- * The mask-repeat.

, maskRepeat
, maskRepeats

-- * The mask-origin.

, maskOrigin
, maskOrigins

-- * The mask-clip.

, maskClip
, maskClips

-- * The mask-attachment.

, maskAttachment
, maskAttachments

-- * The mask-image.

, maskImage
, maskImages

)
where

import Data.Semigroup

import Clay.Background
import Clay.Common
import Clay.Property
import Clay.Stylesheet

pkey :: Val a => Prefixed -> a -> Css
pkey k = prefixed (browsers <> k)

-------------------------------------------------------------------------------

-- | We implement the generic mask property as a type class that accepts
-- multiple value types. This allows us to combine different mask aspects into
-- a shorthand syntax.

class Val a => Mask a where
  mask :: a -> Css
  mask = pkey "mask"

instance Mask a => Mask [a]
instance (Mask a, Mask b) => Mask (a, b)

instance Mask MaskComposite
instance Mask BackgroundPosition
instance Mask BackgroundSize
instance Mask BackgroundRepeat
instance Mask BackgroundOrigin
instance Mask BackgroundClip
instance Mask BackgroundAttachment
instance Mask BackgroundImage

-------------------------------------------------------------------------------

newtype MaskComposite = MaskComposite Value
  deriving (Val, Other, Inherit, None)

clear, copy
  , sourceOver, sourceIn, sourceOut, sourceAtop
  , destinationOver, destinationIn, destinationOut, destinationAtop
  , xor :: MaskComposite

clear                = other "clear"
copy                 = other "copy"
sourceOver           = other "source-over"
sourceIn             = other "source-in"
sourceOut            = other "source-out"
sourceAtop           = other "source-atop"
destinationOver      = other "destination-over"
destinationIn        = other "destination-in"
destinationOut       = other "destination-out"
destinationAtop      = other "destination-atop"
xor                  = other "xor"

maskComposite :: MaskComposite -> Css
maskComposite = pkey "mask-composite"

maskComposites :: [MaskComposite] -> Css
maskComposites = pkey "mask-composite"

-------------------------------------------------------------------------------

maskPosition :: BackgroundPosition -> Css
maskPosition = pkey "mask-position"

maskPositions :: [BackgroundPosition] -> Css
maskPositions = pkey "mask-position"

-------------------------------------------------------------------------------

maskSize :: BackgroundSize -> Css
maskSize = pkey "mask-size"

maskSizes :: [BackgroundSize] -> Css
maskSizes = pkey "mask-size"

-------------------------------------------------------------------------------

maskRepeat :: BackgroundRepeat -> Css
maskRepeat = pkey "mask-repeat"

maskRepeats :: [BackgroundRepeat] -> Css
maskRepeats = pkey "mask-repeat"

-------------------------------------------------------------------------------

maskImage :: BackgroundImage -> Css
maskImage = pkey "mask-image"

maskImages :: [BackgroundImage] -> Css
maskImages = pkey "mask-image"

-------------------------------------------------------------------------------

maskOrigin :: BackgroundOrigin -> Css
maskOrigin = pkey "mask-origin"

maskOrigins :: [BackgroundOrigin] -> Css
maskOrigins = pkey "mask-origin"

-------------------------------------------------------------------------------

maskClip :: BackgroundClip -> Css
maskClip = pkey "mask-clip"

maskClips :: [BackgroundClip] -> Css
maskClips = pkey "mask-clip"

-------------------------------------------------------------------------------

maskAttachment :: BackgroundAttachment -> Css
maskAttachment = pkey "mask-attachment"

maskAttachments :: [BackgroundAttachment] -> Css
maskAttachments = pkey "mask-attachment"

