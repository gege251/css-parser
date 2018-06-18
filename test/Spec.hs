{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CssDocumentSpec
import           GrepSpec
import           Selector
import           SelectorSpec
import           LibSpec
import           Test.Hspec


main :: IO ()
main = do
    hspec $ do
        SelectorSpec.spec
        CssDocumentSpec.spec
        GrepSpec.spec
        LibSpec.spec


