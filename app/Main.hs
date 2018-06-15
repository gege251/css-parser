{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib
import           Control.Monad (mapM_)
import           System.Environment
import           Prelude               hiding (readFile)
import           Data.Text.IO
import           Data.List
import           Selector


main :: IO ()
main = do
    args <- getArgs
    case args of
        (path : []) -> do
            results <- parseCssFile path
            printResults results ""

        (path : option : []) -> do
            results <- parseCssFile path
            printResults results option

        otherwise ->
            return ()


parseCssFile :: String -> IO (Either String [ Selector ])
parseCssFile cssFile = 
    readFile cssFile >>= return . parseCss


printResults :: Either String [ Selector ] -> String -> IO ()
printResults results option =
    case results of
        Left err ->
            print err

        Right selectors ->
            let
                filterOption =
                    case option of
                        "class" -> filter isClass
                        "type" -> filter isType
                        "id" -> filter isId
                        "attribute" -> filter isAttribute
                        "pseudo-element" -> filter isPseudoElement
                        "pseudo-class" -> filter isPseudoClass
                        otherwise -> id
            in
                mapM_ print $ (nub . filterOption) selectors

