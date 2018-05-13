{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Clay.Comments where

import Data.Foldable (foldMap)
import Data.Monoid ((<>))
import Data.Maybe (isNothing)
import Data.List (partition)

import Clay.Stylesheet

-- | Annotate the supplied 'Css' with the supplied comment.
-- Comments work with 'OverloadedStrings'. This will annotate every non-nested
-- value.
commenting :: CommentText -> Css -> Css
commenting c css = foldMap (rule . addComment c) $ runS css
infixl 3 `commenting`

-- The last case indicates there may be something wrong in the typing, as
-- it shouldn't be possible to comment a wrong rule. In practice, this implementation
-- means only the directly applied property rule is affected, i.e. no nested
-- rules. That could be changed by adding recursive cases.
addComment :: CommentText -> Rule -> Rule
addComment c (Property (PartitionComments xs (Just cs)) k v) = let c1 = Comment $ cs <> "; " <> c in
  Property (c1 : xs) k v
addComment c (Property ms k v  ) = Property (Comment c : ms) k v
addComment _ r                   = r

#if __GLASGOW_HASKELL__ >= 710
pattern PartitionComments :: [Modifier] -> Maybe CommentText -> [Modifier]
#endif
pattern PartitionComments xs cs <- (fmap (foldMap _Comment) . partition (isNothing . _Comment) -> (xs, cs))
