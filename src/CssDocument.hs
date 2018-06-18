{-# LANGUAGE OverloadedStrings #-}

module CssDocument
    ( declarationHeader
    , declaration
    , document
    , parseCss
    ) where

import           Control.Applicative              (many, (*>), (<*), (<|>))
import           Control.Monad                    (void)
import           Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char,
                                                   manyTill, notInClass,
                                                   parseOnly, sepBy, skipMany,
                                                   skipSpace, skipWhile, space,
                                                   string)
import           Data.ByteString                  (ByteString)
import           Selector                         (Selector, selector)


parseCss :: ByteString -> Either String [ Selector ]
parseCss css = do
    parseOnly document css


document :: Parser [ Selector ]
document = do
    skipMany atRule
    concat <$> many (junk *> declaration <* junk)


junk :: Parser ()
junk =
    skipMany $ void comment <|> void space


declaration :: Parser [ Selector ]
declaration =
    nestedAtRule <|> simpleDeclaration


simpleDeclaration :: Parser [ Selector ]
simpleDeclaration = do
    selectors <- declarationHeader
    skipSpace
    declarationContent
    return selectors


declarationHeader :: Parser [ Selector ]
declarationHeader =
    selector `sepBy` selectorCombinators


selectorCombinators :: Parser ByteString
selectorCombinators =
    string "," <* skipSpace
    <|> skipSpace *> string ">" <* skipSpace
    <|> skipSpace *> string "+" <* skipSpace
    <|> skipSpace *> string "~" <* skipSpace
    <|> string "" <* skipSpace


declarationContent :: Parser ()
declarationContent = do
    char '{'
    skipSpace
    skipWhile ((/=) '}')
    char '}'
    return ()


comment :: Parser String
comment =
    string "/*" *> manyTill anyChar (string "*/")


atRule :: Parser ()
atRule = do
    char '@'
    skipWhile (notInClass ";{")
    char ';'
    return ()


nestedAtRule :: Parser [ Selector ]
nestedAtRule = do
    char '@'
    skipWhile ((/=) '{')
    char '{'
    skipSpace
    selectors <- many (junk *> declaration <* junk)
    skipSpace
    char '}'
    return $ concat selectors
