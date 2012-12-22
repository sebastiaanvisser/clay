{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Clay.Stylesheet where

import Data.Text (Text)
import Control.Monad.Writer

import Clay.Selector hiding (Child)
import Clay.Property
import Clay.Common

newtype MediaType = MediaType Value
  deriving (Val, Other)

data NotOrOnly = Not | Only
data MediaQuery = MediaQuery (Maybe NotOrOnly) MediaType [Feature]

data Feature = Feature Text (Maybe Value)

----------------------------------------------------

data App
  = Self   Refinement
  | Root   Selector
  | Pop    Int
  | Child  Selector
  | Sub    Selector

data Rule
  = Property (Key ()) Value
  | Nested   App [Rule]
  | Query    MediaQuery [Rule]

newtype StyleM a = S (Writer [Rule] a)
  deriving Monad

-- | The `Css` context is used to collect style rules which are mappings from
-- selectors to style properties. The `Css` type is a computation in the
-- `StyleM` monad that just collects and doesn't return anything.

type Css = StyleM ()

-- | Add a new style property to the stylesheet with the specified `Key` and
-- value. The value can be any type that is in the `Val' typeclass, with other
-- words: can be converted to a `Value`.

key :: Val a => Key a -> a -> Css
key k v = S $ tell [Property (cast k) (value v)]

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
(?) sel (S rs) = S (tell [Nested (Sub sel) (execWriter rs)])

-- | Assign a stylesheet to a selector. When the selector is nested inside an
-- outer scope it will be composed with `|>`.

(<?) :: Selector -> Css -> Css
(<?) sel (S rs) = S (tell [Nested (Child sel) (execWriter rs)])

-- | Assign a stylesheet to a filter selector. When the selector is nested
-- inside an outer scope it will be composed with the `with` selector.

(&) :: Refinement -> Css -> Css
(&) p (S rs) = S (tell [Nested (Self p) (execWriter rs)])

-- | Root is used to add style rules to the top scope.

root :: Selector -> Css -> Css
root sel (S rs) = S (tell [Nested (Root sel) (execWriter rs)])

-- | Pop is used to add style rules to selectors defined in an outer scope. The
-- counter specifies how far up the scope stack we want to add the rules.

pop :: Int -> Css -> Css
pop i (S rs) = S (tell [Nested (Pop i) (execWriter rs)])

-------------------------------------------------------------------------------

-- | Apply a set of style rules when the media type and feature queries apply.

query :: MediaType -> [Feature] -> Css -> Css
query ty fs (S rs) = S (tell [Query (MediaQuery Nothing ty fs) (execWriter rs)])

-- | Apply a set of style rules when the media type and feature queries do not apply.

queryNot :: MediaType -> [Feature] -> Css -> Css
queryNot ty fs (S rs) = S (tell [Query (MediaQuery (Just Not) ty fs) (execWriter rs)])

-- | Apply a set of style rules only when the media type and feature queries apply.

queryOnly :: MediaType -> [Feature] -> Css -> Css
queryOnly ty fs (S rs) = S (tell [Query (MediaQuery (Just Only) ty fs) (execWriter rs)])

