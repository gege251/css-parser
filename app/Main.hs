{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative              (many, (*>), (<|>))
import           Data.Attoparsec.ByteString.Char8 (Parser, char, parseOnly,
                                                   string)
import           Data.ByteString.Char8            (pack)
import           Data.List                        (nub)
import           Lib                              (filterResults, grepSelectors,
                                                   parseCssFile,
                                                   printGrepResults,
                                                   printSelectorList,
                                                   transposeGrepResults)
import           Selector                         (Selector (..), isAttribute,
                                                   isClass, isId, isPseudoClass,
                                                   isPseudoElement, isType)
import           System.Environment               (getArgs)


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

        otherwise ->
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


orFilter :: [ a -> Bool ] -> a -> Bool
orFilter fs a =
    foldl (\acc f -> f a || acc) False fs


data Option
    = Classes
    | Types
    | Ids
    | Attributes
    | PseudoElements
    | PseudoClasses


parseOptions :: String -> Either String [ Option ]
parseOptions options =
    parseOnly optionsParser (pack options)


optionsParser :: Parser [ Option ]
optionsParser =
    char '-' *> many optionParser <|> string "--" *> return []


optionParser :: Parser Option
optionParser =
    char 'c'        *> pure Classes
    <|> char 't'    *> pure Types
    <|> char 'i'    *> pure Ids
    <|> char 'a'    *> pure Attributes
    <|> string "pe" *> pure PseudoElements
    <|> string "pc" *> pure PseudoClasses


isSelector :: Option -> Selector -> Bool
isSelector option =
    case option of
        Classes        -> isClass
        Types          -> isType
        Ids            -> isId
        Attributes     -> isAttribute
        PseudoElements -> isPseudoElement
        PseudoClasses  -> isPseudoClass
