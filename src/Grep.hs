{-# LANGUAGE OverloadedStrings #-}

module Grep (GrepResult, grepAt) where

import           System.IO.Error
import           Data.Text         (Text, count, pack, splitOn)
import           Data.Text.IO      (readFile)
import           Prelude           hiding (readFile, try)
import           System.Directory  (doesDirectoryExist, doesFileExist,
                                    listDirectory)


type GrepResult = ( FileName , [ Int ] )

type FileName = String


grepAt :: Text -> String -> IO (Either String [ GrepResult ])
grepAt query path = do
    fs <- getFileSystemAt path
    case fs of
        Left err         -> return $ Left err
        Right fileSystem -> Right <$> grepFileSystem query fileSystem


grepFileSystem :: Text -> FileSystem -> IO [ GrepResult ]
grepFileSystem query fs =
    case fs of
        File file -> do
            result <- grepFile query file
            if length result == 0
                then return []
                else return [ (file, result) ]

        Directory _ children -> do
            results <- mapM (grepFileSystem query) children
            return $ concat results


grepFile :: Text -> FileName -> IO [ Int ]
grepFile query source = do
    fileContent <- tryIOError (readFile source)
    case fileContent of
        Left err      -> return []
        Right content -> return $ grep query content


grep :: Text -> Text -> [ Int ]
grep query content =
    let
        results =
            init (splitOn query content)

        lineDifferences =
            map (count "\n") results
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
