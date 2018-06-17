{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( parseCss
    , prettify
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
