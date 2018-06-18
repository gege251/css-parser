{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List          (nub)
import           Lib                (filterResults, grepSelectors, orFilter,
                                     parseCssFile, printGrepResults,
                                     printSelectorList, transposeGrepResults)
import           Options            (Option, isSelector, parseOptions)
import           Selector           (Selector)
import           System.Environment (getArgs)


main :: IO ()
main = do
    args <- getArgs
    case args of
        (path : []) -> do
            results <- parseCssFile path
            printSelectorList (filterSelectors [] <$> results)

        (path : args : []) -> do
            results <- parseCssFile path
            let eitherOptions = parseOptions args
            case eitherOptions of
                Left err      -> putStrLn "Invalid arguments"
                Right options -> printSelectorList (filterSelectors options <$> results)

        (path : args : grepTarget : []) -> do
            results <- parseCssFile path
            let eitherOptions = parseOptions args
            case eitherOptions of
                Left err      -> putStrLn "Invalid arguments"
                Right options -> do
                    let filtered = filterSelectors options <$> results
                    grepResults <- grepSelectors grepTarget filtered
                    case grepResults of
                        Left err      -> putStrLn err
                        Right results -> (printGrepResults . filterResults . transposeGrepResults) results

        _ ->
            return ()



-- OPTIONS


filterSelectors :: [ Option ] -> [ Selector ] -> [ Selector ]
filterSelectors [] selectors            = selectors
filterSelectors selectorTypes selectors =
    let
        predicates =
            map isSelector selectorTypes
    in
        (nub . filter (orFilter predicates)) selectors
