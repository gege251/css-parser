{-# LANGUAGE OverloadedStrings #-}

module Selector where

import           Data.Attoparsec.Text
import           Data.Text (Text, unpack)
import           Control.Applicative


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
            TypeSelector text -> unpack text
            IdSelector text -> "#" ++ unpack text
            ClassSelector text -> "." ++ unpack text
            AttributeSelector text -> "[" ++ unpack text ++ "]"
            UniversalSelector -> "*"
            PseudoElement text -> "::" ++ unpack text
            PseudoClass text -> ":" ++ unpack text



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


unallowedChars :: [ Char ]
unallowedChars =
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
    name <- takeWhile1 (notInClass unallowedChars)
    return $ TypeSelector name


idSelector :: Parser Selector
idSelector = do
    char '#'
    name <- takeWhile1 (notInClass unallowedChars)
    return $ IdSelector name


classSelector :: Parser Selector
classSelector = do
    char '.'
    name <- takeWhile1 (notInClass unallowedChars)
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
    name <- takeWhile1 (notInClass unallowedChars)
    return $ PseudoElement name


pseudoClass :: Parser Selector
pseudoClass = do
    char ':'
    name <- takeWhile1 (notInClass unallowedChars)
    return $ PseudoClass name
