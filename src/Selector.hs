{-# LANGUAGE OverloadedStrings #-}

module Selector
    ( Selector (..)
    , selector
    , toPrettyName
    , toName
    , prettyPrint
    , isType
    , isId
    , isClass
    , isAttribute
    , isPseudoClass
    , isPseudoElement
    ) where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8 (Parser, char, inClass,
                                                   satisfy, skipWhile, string,
                                                   takeWhile, takeWhile1)
import           Data.ByteString.Char8            (ByteString, cons, putStrLn)
import           Data.Monoid                      ((<>))
import           Prelude                          hiding (putStrLn, takeWhile)


data Selector
    = TypeSelector ByteString
    | IdSelector ByteString
    | ClassSelector ByteString
    | AttributeSelector ByteString
    | UniversalSelector
    | PseudoElement ByteString
    | PseudoClass ByteString
    deriving (Eq, Ord, Show)


prettyPrint :: Selector -> IO ()
prettyPrint =
    putStrLn . toPrettyName


toPrettyName :: Selector -> ByteString
toPrettyName selector =
        case selector of
            TypeSelector text      -> text
            IdSelector text        -> "#" <> text
            ClassSelector text     -> "." <> text
            AttributeSelector text -> "[" <> text <> "]"
            UniversalSelector      -> "*"
            PseudoElement text     -> "::" <> text
            PseudoClass text       -> ":" <> text


toName :: Selector -> ByteString
toName selector =
        case selector of
            TypeSelector text      -> text
            IdSelector text        -> text
            ClassSelector text     -> text
            AttributeSelector text -> text
            UniversalSelector      -> "*"
            PseudoElement text     -> text
            PseudoClass text       -> text



-- PREDICATES


isType :: Selector -> Bool
isType (TypeSelector _) = True
isType _                = False


isId :: Selector -> Bool
isId (IdSelector _) = True
isId _              = False


isClass :: Selector -> Bool
isClass (ClassSelector _) = True
isClass _                 = False


isAttribute :: Selector -> Bool
isAttribute (AttributeSelector _) = True
isAttribute _                     = False


isPseudoElement :: Selector -> Bool
isPseudoElement (PseudoElement _) = True
isPseudoElement _                 = False


isPseudoClass :: Selector -> Bool
isPseudoClass (PseudoClass _) = True
isPseudoClass _               = False



-- PARSERS


allowedChars :: String
allowedChars =
    "_a-zA-Z0-9-"


allowedFirstChars :: String
allowedFirstChars =
    "_a-zA-Z"


takeAllowedChars :: Parser ByteString
takeAllowedChars = do
    head <- satisfy (inClass allowedFirstChars)
    rest <- takeWhile (inClass allowedChars)
    return $ cons head rest


selector :: Parser Selector
selector =
    classSelector
    <|> idSelector
    <|> attributeSelector
    <|> universalSelector
    <|> pseudoElement
    <|> pseudoClass
    <|> typeSelector


typeSelector :: Parser Selector
typeSelector =
    TypeSelector <$> takeAllowedChars


idSelector :: Parser Selector
idSelector = do
    char '#'
    name <- takeAllowedChars
    return $ IdSelector name


classSelector :: Parser Selector
classSelector = do
    char '.'
    name <- takeAllowedChars
    return $ ClassSelector name


attributeSelector :: Parser Selector
attributeSelector = do
    char '['
    name <- takeWhile1 (']' /=)
    char ']'
    return $ AttributeSelector name


universalSelector :: Parser Selector
universalSelector =
    char '*' *> return UniversalSelector


pseudoElement :: Parser Selector
pseudoElement = do
    string "::"
    name <- takeWhile1 (inClass allowedChars)
    return $ PseudoElement name


pseudoClass :: Parser Selector
pseudoClass = do
    char ':'
    name <- withBrackets <|> takeAllowedChars
    return $ PseudoClass name


withBrackets :: Parser ByteString
withBrackets = do
    name <- takeAllowedChars
    char '('
    skipWhile (')' /=)
    char ')'
    return $ name <> "()"
