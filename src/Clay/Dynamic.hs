{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , NoImplicitPrelude
  #-}

-- | Dynamic user interface element control. This CSS3 functionality is still
-- in draft, though it is implemented in several browsers. See
-- <http://www.w3.org/TR/2000/WD-css3-userint-20000216#dynamic> and your target
-- browsers' vendor documentation for more information.

module Clay.Dynamic
(
  -- * User input
  userInput, UserInput, inputEnabled, inputDisabled

  -- * User modifiability
, userModify, UserModify, readOnly, readWrite, writeOnly

  -- * User selection
, userSelect, UserSelect, selectText, selectToggle, selectElement, selectElements

  -- * User focus
, userFocus, UserFocus, selectBefore, selectAfter, selectSame, selectMenu
)
where

import Clay.Common
import Clay.Property
import Clay.Stylesheet
import Data.Monoid hiding (All)
import Prelude (($))

--------------------------------------------------------------------------------
-- Enabling user interface elements: the 'user-input' property

-- | Enabling user interface elements.

userInput :: UserInput -> Css
userInput = prefixed (browsers <> "user-input")

-- | Selection mode.

newtype UserInput = UserInput Value
  deriving (Val, Inherit, None)

-- | Selection mode.

inputEnabled, inputDisabled :: UserInput

inputEnabled  = UserInput "enabled"
inputDisabled = UserInput "disabled"

--------------------------------------------------------------------------------
-- Modifiability of an element: the 'user-modify' property

-- | Modifiability of an element.

userModify :: UserModify -> Css
userModify = prefixed (browsers <> "user-modify")

-- | Selection mode.

newtype UserModify = UserModify Value
  deriving (Val, Inherit)

-- | Selection mode.

readOnly, readWrite, writeOnly :: UserModify

readOnly  = UserModify "readonly"
readWrite = UserModify "read-write"
writeOnly = UserModify "write-only"

--------------------------------------------------------------------------------
-- Content selection granularity: the 'user-select' property

-- | Content selection granularity.

userSelect :: UserSelect -> Css
userSelect = prefixed (browsers <> "user-select")

-- | Selection mode.

newtype UserSelect = UserSelect Value
  deriving (Val, Inherit, None, All)

-- | Selection mode.

selectText, selectToggle, selectElement, selectElements :: UserSelect

selectText     = UserSelect "text"
selectToggle   = UserSelect "toggle"
selectElement  = UserSelect "element"
selectElements = UserSelect "elements"

--------------------------------------------------------------------------------
-- Focus selection behavior of the contents of an element: the 'user-focus' property

-- | Content focusing granularity.

userFocus :: UserFocus -> Css
userFocus = prefixed (browsers <> "user-focus")

-- | Focus behaviour.

newtype UserFocus = UserFocus Value
  deriving (Val, Inherit, None, Normal, Auto)

instance All UserFocus where all = UserFocus "select-all"

-- | Focus mode.

selectBefore, selectAfter, selectSame, selectMenu :: UserFocus

selectBefore = UserFocus "select-before"
selectAfter  = UserFocus "select-after"
selectSame   = UserFocus "select-same"
selectMenu   = UserFocus "select-menu"
