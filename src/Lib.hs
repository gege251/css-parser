{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( GrepResult
    , PrintableGrepResult
    , parseCssFile
    , printSelectorList
    , printGrepResults
    , filterResults
    , transposeGrepResults
    , grepSelectors
    , orFilter
    ) where

import           CssDocument           (parseCss)
import           Data.ByteString.Char8 (readFile, unpack)
import           Data.List             (groupBy, sortBy)
import           Grep                  (FileName, GrepResult, Query, grepAt)
import           Prelude               hiding (readFile)
import           Selector              (Selector, prettyPrint, toName)
import           System.Console.ANSI   (Color (..), ColorIntensity (..),
                                        ConsoleLayer (..),
                                        SGR (Reset, SetColor), setSGR)


type PrintableGrepResult = (Query, [ (FileName, [ Int ]) ])


parseCssFile :: String -> IO (Either String [ Selector ])
parseCssFile cssFile =
    readFile cssFile >>= return . parseCss


printSelectorList :: Either String [ Selector ] -> IO ()
printSelectorList results =
    case results of
        Left err        -> print err
        Right selectors -> mapM_ prettyPrint selectors


printGrepResults :: [ PrintableGrepResult ] -> IO ()
printGrepResults =
    let
        printPerGrepResult ( filename, lineNums ) =
            putStrLn $ "    " ++ filename ++ (show lineNums)

        printPerSelector ( selector, grepResult ) = do
            setSGR [ SetColor Foreground Vivid Yellow ]
            putStrLn $ unpack selector
            setSGR [ Reset ]
            mapM_ printPerGrepResult grepResult
    in
        mapM_ printPerSelector


transposeGrepResults :: [ (FileName, [ (Query, [ Int ]) ]) ] -> [ (Query, [ (FileName, [ Int ]) ]) ]
transposeGrepResults =
     groupBySnd . concat . (map flatten)


flatten :: ( a, [ (b,c) ]) -> [ (a,b,c) ]
flatten (a, bcs) =
    map (\(b, c) -> (a, b, c)) bcs


groupBySnd :: (Eq b, Ord b) => [ (a,b,c) ] -> [ (b, [ (a,c) ]) ]
groupBySnd =
    let
        sortByB :: Ord b => [ (a,b,c) ] -> [ (a,b,c) ]
        sortByB  = sortBy (\(_,a,_) (_,b,_) -> compare a b)

        groupByB :: Eq b => [ (a,b,c) ] -> [ [ (a,b,c) ] ]
        groupByB = groupBy (\(_,a,_) (_,b,_) -> a == b)

        pullOutB :: [ (a,b,c) ] -> (b, [ (a,c) ])
        pullOutB list =
            let
                getB  (_,b,_) = b
                getAC (a,_,c) = (a,c)
            in
                ( ((getB . head) list), map getAC list )
    in
        (map pullOutB) . groupByB . sortByB


filterResults :: Bool -> [ PrintableGrepResult ] -> [ PrintableGrepResult ]
filterResults unusedOnly =
    let
        filterEmpty :: [ (FileName, [ Int ]) ] -> [ (FileName, [ Int ]) ]
        filterEmpty =
            filter (\ (filename, lineNums) -> length lineNums /= 0)

        emptyFileFilter =
            map (\ (query, results) -> (query, filterEmpty results))

        unusedSelectorFilter =
            if unusedOnly
                then filter (\ (query, files) -> length files == 0)
                else id
    in
        unusedSelectorFilter . emptyFileFilter




grepSelectors :: String -> Either String [ Selector ] -> IO ( Either String [ GrepResult ] )
grepSelectors path eitherSelectors =
    case eitherSelectors of
        Left err -> return $ Left err
        Right selectors -> do
            grepResults <- grepAt path (map toName selectors)
            return $ grepResults


orFilter :: [ a -> Bool ] -> a -> Bool
orFilter fs a =
    foldl (\acc f -> f a || acc) False fs
