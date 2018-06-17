{-# LANGUAGE OverloadedStrings #-}

module Selector where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text (Parser, char, notInClass, string,
                                       takeWhile1)
import           Data.Text            (Text, unpack)


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


disallowedChars :: [ Char ]
disallowedChars =
    " {}@.:,[]"


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
    name <- takeWhile1 (notInClass disallowedChars)
    return $ TypeSelector name


idSelector :: Parser Selector
idSelector = do
    char '#'
    name <- takeWhile1 (notInClass disallowedChars)
    return $ IdSelector name


classSelector :: Parser Selector
classSelector = do
    char '.'
    name <- takeWhile1 (notInClass disallowedChars)
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
    name <- takeWhile1 (notInClass disallowedChars)
    return $ PseudoElement name


pseudoClass :: Parser Selector
pseudoClass = do
    char ':'
    name <- takeWhile1 (notInClass disallowedChars)
    return $ PseudoClass name
