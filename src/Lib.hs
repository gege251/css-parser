{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( declarationHeader
    , declaration
    , document
    , parseCss
    , Selector(..)
    ) where

import           Prelude hiding (takeWhile)
import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Text (Text, unpack)
import           Selector


parseCss :: Text -> Either String [ Selector ]
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
declarationHeader = do
    selector `sepBy` selectorCombinators


selectorCombinators :: Parser Text
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
