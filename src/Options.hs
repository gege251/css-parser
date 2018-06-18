{-# LANGUAGE OverloadedStrings #-}

module Options where

import           Data.ByteString.Char8 (pack)
import           Data.Maybe            (catMaybes)
import           Data.Semigroup        ((<>))
import           Options.Applicative   (Parser, help, long, metavar, short,
                                        strOption, switch, value)
import           Selector              (Selector, isAttribute, isClass, isId,
                                        isPseudoClass, isPseudoElement, isType)


data Options = Options
    { cssPath              :: String
    , sourcePath           :: String
    , filterClasses        :: Bool
    , filterTypes          :: Bool
    , filterIds            :: Bool
    , filterAttributes     :: Bool
    , filterPseudoElements :: Bool
    , filterPseudoClasses  :: Bool
    }
    deriving Show


options :: Parser Options
options = Options
    <$> strOption
        ( long "file"
        <> short 'f'
        <> metavar "FILE"
        <> help "Input file for the css parser" )
    <*> strOption
        ( long "source"
        <> short 's'
        <> value ""
        <> metavar "PATH"
        <> help "Source file path to grep in" )
    <*> switch
        ( short 'c'
        <> help "Filter for classes" )
    <*> switch
        ( short 't'
        <> help "Filter for types" )
    <*> switch
        ( short 'i'
        <> help "Filter for ids" )
    <*> switch
        ( short 'a'
        <> help "Filter for attributes" )
    <*> switch
        ( short 'e'
        <> help "Filter for pseudo-elements" )
    <*> switch
        ( short 'p'
        <> help "Filter for pseudo-classes" )


toPredicateList :: Options -> [ ( Selector -> Bool ) ]
toPredicateList options =
    catMaybes
        [ if filterClasses options        then Just isClass         else Nothing
        , if filterTypes options          then Just isType          else Nothing
        , if filterIds options            then Just isId            else Nothing
        , if filterAttributes options     then Just isAttribute     else Nothing
        , if filterPseudoElements options then Just isPseudoElement else Nothing
        , if filterPseudoClasses options  then Just isPseudoClass   else Nothing
        ]
