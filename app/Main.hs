{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative  (many, (*>), (<|>))
import           Control.Monad        (mapM_)
import           Data.Attoparsec.Text (Parser, char, parseOnly, string)
import           Data.List            (nub)
import           Data.Text            (Text, pack)
import           Data.Text.IO         (readFile)
import           Lib                  (Selector (..), isAttribute, isClass,
                                       isId, isPseudoClass, isPseudoElement,
                                       isType, parseCss, prettyPrint)
import           Prelude              hiding (readFile)
import           System.Environment   (getArgs)


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
printResults results optionString =
    case results of
        Left err ->
            print err

        Right selectors ->
            let
                optionalFilter =
                    case parseOnly optionsParser (pack optionString) of
                        Right selectorTypes ->
                            let
                                predicates =
                                    map isSelector selectorTypes
                            in
                                filter (orFilter predicates)
                        _ -> id
            in
                mapM_ prettyPrint $ (nub . optionalFilter) selectors


orFilter :: [ a -> Bool ] -> a -> Bool
orFilter fs a =
    foldl (\acc f -> f a || acc) False fs


-- OPTION PARSERS


data Option
    = Classes
    | Types
    | Ids
    | Attributes
    | PseudoElements
    | PseudoClasses


optionsParser :: Parser [ Option ]
optionsParser =
    char '-' *> many optionParser


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
