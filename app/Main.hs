{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative              (many, (*>), (<|>))
import           Control.Monad                    (mapM, mapM_)
import           Data.Attoparsec.ByteString.Char8 (Parser, char, parseOnly,
                                                   string)
import           Data.ByteString.Char8            (ByteString, pack, readFile)
import           Data.List                        (nub)
import           Grep                             (GrepResult, grepAt)
import           Lib                              (Selector (..), isAttribute,
                                                   isClass, isId, isPseudoClass,
                                                   isPseudoElement, isType,
                                                   parseCss, prettify,
                                                   prettyPrint)
import           Prelude                          hiding (readFile)
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
                        Right results -> printGrepResults results

        otherwise ->
            return ()


parseCssFile :: String -> IO (Either String [ Selector ])
parseCssFile cssFile =
    readFile cssFile >>= return . parseCss


printSelectorList :: Either String [ Selector ] -> IO ()
printSelectorList results =
    case results of
        Left err        -> print err
        Right selectors -> mapM_ prettyPrint selectors


printGrepResults :: [( Selector, [ GrepResult ] )] -> IO ()
printGrepResults results =
    let
        printPerGrepResult ( filename, lineNums ) =
            putStrLn $ "    " ++ filename ++ (show lineNums)

        printPerSelector ( selector, grepResult ) = do
            prettyPrint selector
            mapM_ printPerGrepResult grepResult
    in
        mapM_ printPerSelector results


grepSelectors :: String -> Either String [ Selector ] -> IO (Either String [( Selector, [ GrepResult ] )])
grepSelectors path eitherSelectors =
    case eitherSelectors of
        Left err -> return $ Left err
        Right selectors -> do
            grepResults <- mapM ((flip grepAt path) . prettify) selectors
            return $ fmap (zip selectors) $ sequence grepResults


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
