{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Clay.Stylesheet where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Foldable (foldMap)
import Data.Maybe (isJust)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.String (IsString)
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

newtype CommentText = CommentText { unCommentText :: Text }
  deriving (Show, IsString, Semigroup, Monoid)

data Modifier
  = Important
  | Comment CommentText
  deriving (Show)

_Important :: Modifier -> Maybe Text
_Important Important   = Just "!important"
_Important (Comment _) = Nothing

_Comment :: Modifier -> Maybe CommentText
_Comment (Comment c) = Just c
_Comment Important   = Nothing

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
  = Property [Modifier] (Key ()) Value
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

instance Semigroup Css where
  (<>) = liftA2 (<>)

instance Monoid Css where
  mempty = pure ()
  mappend = (<>)

-- | Add a new style property to the stylesheet with the specified `Key` and
-- value. The value can be any type that is in the `Val' typeclass, with other
-- words: can be converted to a `Value`.

key :: Val a => Key a -> a -> Css
key k v = rule $ Property [] (cast k) (value v)

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

-------------------------------------------------------------------------------

-- | Indicate the supplied css should override css declarations that would
-- otherwise take precedence.
--
-- Use sparingly.
important :: Css -> Css
important = foldMap (rule . addImportant) . runS

-- The last case indicates there may be something wrong in the typing, as
-- it shouldn't be possible to make a non-property important. In practice,
-- this implementation means only the directly applied property rule is
-- affected, i.e. no nested rules. That could be changed by adding recursive cases.
addImportant :: Rule -> Rule
addImportant (Property ms@(filter (isJust . _Important) -> (_:_)) k v) =
  Property ms k v
addImportant (Property ms k v  ) = Property (Important : ms) k v
addImportant r                   = r
