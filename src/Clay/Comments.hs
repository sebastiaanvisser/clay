{-# LANGUAGE
    GeneralizedNewtypeDeriving
  #-}

module Clay.Comments where

import Data.Monoid
import Data.Semigroup
import Data.String (IsString)
import Data.Text (Text)

newtype CommentText = CommentText { unCommentText :: Text }
  deriving (Show, IsString, Semigroup, Monoid)
