{-# LANGUAGE
    GeneralizedNewtypeDeriving
  #-}

module Clay.Comments where

import Data.String (IsString)
import Data.Text (Text)

newtype CommentText = CommentText { unCommentText :: Text }
  deriving (Show, IsString)
