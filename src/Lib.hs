{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( parseCss
    , prettyPrint
    , Selector(..)
    , isAttribute
    , isClass
    , isId
    , isPseudoClass
    , isPseudoElement
    , isType
    ) where

import           CssDocument
import           Selector
