{-# LANGUAGE OverloadedStrings #-}

module Selector
    ( Selector (..)
    , selector
    , prettify
    , prettyPrint
    , isType
    , isId
    , isClass
    , isAttribute
    , isPseudoClass
    , isPseudoElement
    ) where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text (Parser, char, inClass, notInClass,
                                       satisfy, string, takeWhile, takeWhile1)
import           Data.Text            (Text, cons, unpack)
import           Prelude              hiding (takeWhile)


data Selector
    = TypeSelector Text
    | IdSelector Text
    | ClassSelector Text
    | AttributeSelector Text
    | UniversalSelector
    | PseudoElement Text
    | PseudoClass Text
    deriving (Eq, Show)


prettyPrint :: Selector -> IO ()
prettyPrint =
    putStrLn . prettify


prettify :: Selector -> String
prettify selector =
        case selector of
            TypeSelector text      -> unpack text
            IdSelector text        -> "#" ++ unpack text
            ClassSelector text     -> "." ++ unpack text
            AttributeSelector text -> "[" ++ unpack text ++ "]"
            UniversalSelector      -> "*"
            PseudoElement text     -> "::" ++ unpack text
            PseudoClass text       -> ":" ++ unpack text



-- PREDICATES


isType :: Selector -> Bool
isType (TypeSelector _) = True
isType otherwise        = False


isId :: Selector -> Bool
isId (IdSelector _) = True
isId otherwise      = False


isClass :: Selector -> Bool
isClass (ClassSelector _) = True
isClass otherwise         = False


isAttribute :: Selector -> Bool
isAttribute (AttributeSelector _) = True
isAttribute otherwise             = False


isPseudoElement :: Selector -> Bool
isPseudoElement (PseudoElement _) = True
isPseudoElement otherwise         = False


isPseudoClass :: Selector -> Bool
isPseudoClass (PseudoClass _) = True
isPseudoClass otherwise       = False



-- PARSERS


allowedChars :: [ Char ]
allowedChars =
    "_a-zA-Z0-9-"


allowedFirstChars :: [ Char ]
allowedFirstChars =
    "_a-zA-Z"


takeAllowedChars :: Parser Text
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
typeSelector = do
    name <- takeAllowedChars
    return $ TypeSelector name


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
    name <- takeWhile1 (notInClass "]")
    char ']'
    return $ AttributeSelector name


universalSelector :: Parser Selector
universalSelector =
    char '*' *> return UniversalSelector


pseudoElement :: Parser Selector
pseudoElement = do
    string "::"
    name <- takeAllowedChars
    return $ PseudoElement name


pseudoClass :: Parser Selector
pseudoClass = do
    char ':'
    name <- takeAllowedChars
    return $ PseudoClass name
