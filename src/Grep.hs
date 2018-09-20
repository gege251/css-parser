{-# LANGUAGE OverloadedStrings #-}

module Grep (GrepResult, FileName, grepAt, grep) where

import           Data.ByteString.Char8  (ByteString, count, pack, readFile)
import           Data.ByteString.Search (split)
import           Prelude                hiding (readFile, try)
import           Selector               (Selector, toName)
import           System.Directory       (doesDirectoryExist, doesFileExist,
                                         listDirectory)
import           System.IO.Error


type GrepResult = ( FileName, [( Selector, [ Int ]) ])

type FileName = String


grepAt :: String -> [ Selector ] -> IO (Either String [ GrepResult ])
grepAt path queries = do
    fs <- getFileSystemAt path
    case fs of
        Left err         -> return $ Left err
        Right fileSystem -> Right <$> grepFileSystem queries fileSystem


grepFileSystem :: [ Selector ] -> FileSystem -> IO [ GrepResult ]
grepFileSystem queries fs =
    case fs of
        File file -> do
            result <- grepFile queries file
            if length result == 0
                then return []
                else return [ (file, result) ]

        Directory _ children ->
            concat <$> mapM (grepFileSystem queries) children


grepFile :: [ Selector ] -> FileName -> IO [ ( Selector, [ Int ] ) ]
grepFile queries source = do
    fileContent <- tryIOError (readFile source)
    case fileContent of
        Left err      -> return []
        Right content -> return $ zip queries $ map (flip grep content) queries


grep :: Selector -> ByteString -> [ Int ]
grep query content =
    let
        results =
            init (split (toName query) content)

        lineDifferences =
            map (count '\n') results
    in
        (reverse . init) (foldl (\x y -> (y + head x) : x) [1] lineDifferences)



-- FILE SYSTEM FUNCTIONS


data FileSystem
    = Directory String [ FileSystem ]
    | File String
    deriving (Show)


getFileSystemAt :: String -> IO (Either String FileSystem)
getFileSystemAt path = do
    isDirectory <- doesDirectoryExist path
    isFile <- doesFileExist path

    if isDirectory then do
        paths <- listDirectory path
        results <- mapM (getFileSystemAt . toAbsolutePath path) paths
        case sequence results of
            Left err       -> return $ Left err
            Right children -> return $ Right (Directory path children)

    else if isFile then
        return $ Right (File path)

    else
        return $ Left ("unable to read path: " ++ path)


toAbsolutePath :: String -> String -> String
toAbsolutePath root path =
    if root /= "" && last root == '/'
        then root ++ path
        else root ++ "/" ++ path
