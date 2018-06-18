{-# LANGUAGE OverloadedStrings #-}

module Options where

import           Control.Applicative              (some, (*>), (<|>))
import           Data.Attoparsec.ByteString.Char8 (Parser, char, parseOnly,
                                                   string)
import           Data.ByteString.Char8            (pack)
import           Selector                         (Selector (..), isAttribute,
                                                   isClass, isId, isPseudoClass,
                                                   isPseudoElement, isType)


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
    char '-' *> some optionParser <|> string "--" *> return []


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
