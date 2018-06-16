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
                        "c" -> filter isClass
                        "t" -> filter isType
                        "i" -> filter isId
                        "a" -> filter isAttribute
                        "pe" -> filter isPseudoElement
                        "pc" -> filter isPseudoClass
                        otherwise -> id
            in
                mapM_ prettyPrint $ (nub . filterOption) selectors

