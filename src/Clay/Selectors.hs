{-# LANGUAGE OverloadedStrings #-}
module Clay.Selectors where

import Data.Text (Text)

import Clay.Selector

-- List of specific pseudo classes, from:
-- https://developer.mozilla.org/en-US/docs/CSS/Pseudo-classes

link, visited, active, hover, focus, firstChild :: Filter

link       = ":link"
visited    = ":visited"
active     = ":active"
hover      = ":hover"
focus      = ":focus"
firstChild = ":first-child"

firstOfType, lastOfType, empty, target, checked, enabled, disabled :: Filter

firstOfType = ":first-of-type"
lastOfType  = ":last-of-type"
empty       = ":empty"
target      = ":target"
checked     = ":checked"
enabled     = ":enabled"
disabled    = ":disabled"

nthChild, nthLastChild, nthOfType :: Text -> Filter

nthChild     n = func "nth-child"      [n]
nthLastChild n = func "nth-last-child" [n]
nthOfType    n = func "nth-of-type"    [n]

