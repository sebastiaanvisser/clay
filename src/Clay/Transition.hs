{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  #-}
module Clay.Transition
(

-- * The transition propery.

  transition
, transitions

-- * Transition-property.

, transitionProperty
, transitionProperties

-- * Transition-duration.

, transitionDuration
, transitionDurations

-- * Transition-timing-function.

, TimingFunction
, transitionTimingFunction
, ease, easeIn, easeOut, easeInOut, easeLinear, stepStart, stepStop
, stepsStart, stepsStop
, cubicBezier

-- * Transition-delay.
, transitionDelay
, transitionDelays

)
where

import Data.Monoid
import Data.Text (Text)

import Clay.Common
import Clay.Property
import Clay.Stylesheet
import Clay.Time

transition :: Text -> Time -> TimingFunction -> Time -> Css
transition p d f e = prefixed (browsers <> "transition") (p ! d ! f ! e)

transitions :: [(Text, Time, TimingFunction, Time)] -> Css
transitions = prefixed (browsers <> "transition")
            . map (\(p, d, f, e) -> value (p ! d ! f ! e))

-------------------------------------------------------------------------------

transitionProperty :: Text -> Css
transitionProperty = key "transition-property"

transitionProperties :: [Text] -> Css
transitionProperties = key "transition-property"

-------------------------------------------------------------------------------

transitionDuration :: Time -> Css
transitionDuration = key "transition-duration"

transitionDurations :: [Time] -> Css
transitionDurations = key "transition-duration"

-------------------------------------------------------------------------------

newtype TimingFunction = TimingFunction Value
  deriving (Val, Other, Auto)

ease, easeIn, easeOut, easeInOut, easeLinear, stepStart, stepStop :: TimingFunction

ease       = other "ease"
easeIn     = other "easeIn"
easeOut    = other "easeOut"
easeInOut  = other "easeInOut"
easeLinear = other "easeLinear"
stepStart  = other "stepStart"
stepStop   = other "stepStop"

stepsStart, stepsStop :: Integer -> TimingFunction

stepsStart s = other ("steps(" <> value s <> ", end)")
stepsStop  s = other ("steps(" <> value s <> ", end)")

cubicBezier :: Double -> Double -> Double -> Double -> TimingFunction
cubicBezier a b c d = other ("cubic-bezier(" <> value (a ! b ! c ! d) <> ")")

transitionTimingFunction :: TimingFunction -> Css
transitionTimingFunction = key "transition-timing-function"

-------------------------------------------------------------------------------

transitionDelay :: Time -> Css
transitionDelay = key "transition-delay"

transitionDelays :: [Time] -> Css
transitionDelays = key "transition-delay"

