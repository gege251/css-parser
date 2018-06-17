{-# LANGUAGE OverloadedStrings #-}

module SelectorSpec where

import           Data.Text
import           Selector
import           Test.Hspec
import           Test.Hspec.Attoparsec


spec :: Spec
spec =
    describe "parse selectors" $ do
        it "should parse class selector" $
            (".test-class" :: Text)
                ~> selector `shouldParse`
                    ( ClassSelector "test-class" )

        it "should fail on ill named selector" $
                selector `shouldFailOn` (".-class" :: Text)

        it "should parse type selector" $
            ("div" :: Text)
                ~> selector `shouldParse`
                    ( TypeSelector "div" )

        it "should parse id selector" $
            ("#test_id" :: Text)
                ~> selector `shouldParse`
                    ( IdSelector "test_id" )

        it "should parse attribute selector" $
            ("[href*=\"example\"]" :: Text)
                ~> selector `shouldParse`
                    ( AttributeSelector "href*=\"example\"" )

        it "should parse universal selector" $
            ("*" :: Text)
                ~> selector `shouldParse`
                    ( UniversalSelector )
