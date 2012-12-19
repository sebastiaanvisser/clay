{-# LANGUAGE OverloadedStrings #-}
module Clay.Pseudo where

import Data.Text (Text)

import Clay.Selector

-- List of specific pseudo classes, from:
-- https://developer.mozilla.org/en-US/docs/CSS/Pseudo-classes

after, before :: Refinement

after  = ":after"
before = ":before"

link, visited, active, hover, focus, firstChild :: Refinement

link       = ":link"
visited    = ":visited"
active     = ":active"
hover      = ":hover"
focus      = ":focus"
firstChild = ":first-child"

firstOfType, lastOfType, empty, target, checked, enabled, disabled :: Refinement

firstOfType = ":first-of-type"
lastOfType  = ":last-of-type"
empty       = ":empty"
target      = ":target"
checked     = ":checked"
enabled     = ":enabled"
disabled    = ":disabled"

nthChild, nthLastChild, nthOfType :: Text -> Refinement

nthChild     n = func "nth-child"      [n]
nthLastChild n = func "nth-last-child" [n]
nthOfType    n = func "nth-of-type"    [n]

