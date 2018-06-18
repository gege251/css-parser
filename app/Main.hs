{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List           (nub)
import           Data.Semigroup      ((<>))
import           Lib                 (GrepResult, filterResults, grepSelectors,
                                      orFilter, parseCssFile, printGrepResults,
                                      printSelectorList, transposeGrepResults)
import           Options             (Options, cssPath, options, sourcePath,
                                      toPredicateList, unusedOnly)
import           Options.Applicative (execParser, fullDesc, header, helper,
                                      info, progDesc, (<**>))
import           Selector            (Selector)


main :: IO ()
main = runApp =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Grep for css selectors in your project folder"
     <> header "css-parser - CSS parser and grepper" )


runApp :: Options -> IO ()
runApp options = do
    cssSelectors <- parseCssFile (cssPath options)
    if sourcePath options == "" then
        printSelectorList (filterSelectors options <$> cssSelectors)
    else do
        let filtered = filterSelectors options <$> cssSelectors
        grepResults <- grepSelectors (sourcePath options) filtered
        case grepResults of
            Left err      -> putStrLn err
            Right results -> grepPrint options results



-- OPTIONS


grepPrint :: Options -> [ GrepResult ] -> IO ()
grepPrint options =
    printGrepResults . (filterResults (unusedOnly options)) . transposeGrepResults


filterSelectors :: Options -> [ Selector ] -> [ Selector ]
filterSelectors options =
    let
        selectorTypeFilter =
            case toPredicateList options of
                []         -> id
                predicates -> filter (orFilter predicates)
    in
        selectorTypeFilter . nub
