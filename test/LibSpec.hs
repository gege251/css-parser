{-# LANGUAGE OverloadedStrings #-}

module LibSpec where

import           Data.ByteString.Char8
import           Lib
import           Test.Hspec


spec :: Spec
spec =
    describe "Lib functions" $ do
        it "should transpose grep results to a printable form (group by query)" $
            transposeGrepResults
                [ ("file1", [ ("test-class1", [1,11,22]) ])
                , ("file2", [ ("test-class1", [11,110]) ])
                ]
            `shouldBe`
                [ ("test-class1", [ ("file1", [1,11,22]), ("file2", [11,110]) ] )]

        it "should filter empty files from results" $
            filterResults
                [ ("test-class1", [ ("file1", [1,11,22]), ("file2", []) ] )]
            `shouldBe`
                [ ("test-class1", [ ("file1", [1,11,22])] )]

