{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Selector
import           Test.Hspec
import           CssDocumentSpec
import           SelectorSpec


main :: IO ()
main = do
    hspec $ do
        SelectorSpec.spec
        CssDocumentSpec.spec


