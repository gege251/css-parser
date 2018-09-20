{-# LANGUAGE OverloadedStrings #-}

module LibSpec where

import           Data.ByteString.Char8
import           Lib
import           Test.Hspec
import           Selector (Selector(..))


spec :: Spec
spec =
    describe "Lib functions" $ do
        it "should transpose grep results to a printable form (group by query)" $
            transposeGrepResults
                [ ("file1", [ (ClassSelector "test-class1", [1,11,22]) ])
                , ("file2", [ (ClassSelector "test-class1", [11,110]) ])
                ]
            `shouldBe`
                [ (ClassSelector "test-class1", [ ("file1", [1,11,22]), ("file2", [11,110]) ] )]

        it "should filter empty files from results" $
            filterResults
                False [ (ClassSelector "test-class1", [ ("file1", [1,11,22]), ("file2", []) ] )]
            `shouldBe`
                [ (ClassSelector "test-class1", [ ("file1", [1,11,22])] )]

