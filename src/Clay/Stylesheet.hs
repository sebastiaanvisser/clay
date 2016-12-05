{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Clay.Stylesheet where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.Writer hiding (All)
import Data.Text (Text)

import Clay.Selector hiding (Child)
import Clay.Property
import Clay.Common

-------------------------------------------------------------------------------

newtype MediaType = MediaType Value
  deriving (Val, Other, Show, All)

data NotOrOnly = Not | Only
  deriving Show

data MediaQuery = MediaQuery (Maybe NotOrOnly) MediaType [Feature]
  deriving Show

data Feature = Feature Text (Maybe Value)
  deriving Show

-------------------------------------------------------------------------------

data App
  = Self   Refinement
  | Root   Selector
  | Pop    Int
  | Child  Selector
  | Sub    Selector
  deriving Show

data Keyframes = Keyframes Text [(Double, [Rule])]
  deriving Show

data Rule
  = Property (Key ()) Value
  | Nested   App [Rule]
  | Query    MediaQuery [Rule]
  | Face     [Rule]
  | Keyframe Keyframes
  | Import   Text
  deriving Show

newtype StyleM a = S (Writer [Rule] a)
  deriving (Functor, Applicative, Monad)

runS :: Css -> [Rule]
runS (S a) = execWriter a

rule :: Rule -> Css
rule a = S (tell [a])

-- | The `Css` context is used to collect style rules which are mappings from
-- selectors to style properties. The `Css` type is a computation in the
-- `StyleM` monad that just collects and doesn't return anything.

type Css = StyleM ()

instance Monoid Css where
  mempty = pure ()
  mappend = liftA2 mappend

-- | Add a new style property to the stylesheet with the specified `Key` and
-- value. The value can be any type that is in the `Val' typeclass, with other
-- words: can be converted to a `Value`.

key :: Val a => Key a -> a -> Css
key k v = rule $ Property (cast k) (value v)

-- | Add a new style property to the stylesheet with the specified `Key` and
-- value, like `key` but use a `Prefixed` key.

prefixed :: Val a => Prefixed -> a -> Css
prefixed xs = key (Key xs)

infix 4 -:

-- | The colon operator can be used to add style rules to the current context
-- for which there is no embedded version available. Both the key and the value
-- are plain text values and rendered as is to the output CSS.

(-:) :: Key Text -> Text -> Css
(-:) = key

-------------------------------------------------------------------------------

infixr 5 <?
infixr 5 ?
infixr 5 &

-- | Assign a stylesheet to a selector. When the selector is nested inside an
-- outer scope it will be composed with `deep`.

(?) :: Selector -> Css -> Css
(?) sel rs = rule $ Nested (Sub sel) (runS rs)

-- | Assign a stylesheet to a selector. When the selector is nested inside an
-- outer scope it will be composed with `|>`.

(<?) :: Selector -> Css -> Css
(<?) sel rs = rule $ Nested (Child sel) (runS rs)

-- | Assign a stylesheet to a filter selector. When the selector is nested
-- inside an outer scope it will be composed with the `with` selector.

(&) :: Refinement -> Css -> Css
(&) p rs = rule $ Nested (Self p) (runS rs)

-- | Root is used to add style rules to the top scope.

root :: Selector -> Css -> Css
root sel rs = rule $ Nested (Root sel) (runS rs)

-- | Pop is used to add style rules to selectors defined in an outer scope. The
-- counter specifies how far up the scope stack we want to add the rules.

pop :: Int -> Css -> Css
pop i rs = rule $ Nested (Pop i) (runS rs)

-------------------------------------------------------------------------------

-- | Apply a set of style rules when the media type and feature queries apply.

query :: MediaType -> [Feature] -> Css -> Css
query ty fs rs = rule $ Query (MediaQuery Nothing ty fs) (runS rs)

-- | Apply a set of style rules when the media type and feature queries do not apply.

queryNot :: MediaType -> [Feature] -> Css -> Css
queryNot ty fs rs = rule $ Query (MediaQuery (Just Not) ty fs) (runS rs)

-- | Apply a set of style rules only when the media type and feature queries apply.

queryOnly :: MediaType -> [Feature] -> Css -> Css
queryOnly ty fs rs = rule $ Query (MediaQuery (Just Only) ty fs) (runS rs)

-------------------------------------------------------------------------------

keyframes :: Text -> [(Double, Css)] -> Css
keyframes n xs = rule $ Keyframe (Keyframes n (map (second runS) xs))

keyframesFromTo :: Text -> Css -> Css -> Css
keyframesFromTo n a b = keyframes n [(0, a), (100, b)]

-------------------------------------------------------------------------------

-- | Define a new font-face.

fontFace :: Css -> Css
fontFace rs = rule $ Face (runS rs)


-- | Import a CSS file from a URL

importUrl :: Text -> Css
importUrl l = rule $ Import l

