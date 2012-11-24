{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rules where

import Data.Text (Text)
import Control.Monad.Writer

import Selector

data Property = Property
  { key   :: Text
  , value :: Text
  }

data Rule = Rule
  { selector   :: Selector
  , properties :: [Property]
  }

type Prop = Writer [Property] ()

(-:) :: Text -> Text -> Prop
(-:) k v = tell [Property k v]

rules :: Selector -> Prop -> [Rule]
rules sel rs = [Rule sel (execWriter rs)]

