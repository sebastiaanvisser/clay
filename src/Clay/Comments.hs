module Clay.Comments where

import Data.Monoid ((<>))

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
addComment c (Property Nothing k v  ) = Property (Just c) k v
addComment c (Property (Just c0) k v) = Property (Just $ c <> c0) k v
addComment _ r                        = r

