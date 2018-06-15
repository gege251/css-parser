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
    skipMany header
    concat <$> many (junk *> declaration <* junk)


junk :: Parser ()
junk =
    skipMany $ void comment <|> void space


selectorCombinators :: Parser Text
selectorCombinators =
    string "," <* skipSpace
    <|> skipSpace *> string ">" <* skipSpace
    <|> skipSpace *> string "+" <* skipSpace
    <|> skipSpace *> string "~" <* skipSpace
    <|> string "" <* skipSpace


declarationHeader :: Parser [ Selector ]
declarationHeader = do
    selector `sepBy` selectorCombinators


declaration :: Parser [ Selector ]
declaration = do
    skipSpace
    selectors <- declarationHeader
    skipSpace
    declarationContent
    skipSpace
    return selectors


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


header :: Parser ()
header = do
    char '@'
    skipWhile ((/=) ';')
    char ';'
    return ()
