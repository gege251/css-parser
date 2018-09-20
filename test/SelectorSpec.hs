{-# LANGUAGE OverloadedStrings #-}

module SelectorSpec where

import           Data.ByteString.Char8
import           Selector
import           Test.Hspec
import           Test.Hspec.Attoparsec


spec :: Spec
spec =
    describe "parse selectors" $ do
        it "should parse class selector" $
            (".test-class" :: ByteString)
                ~> selector `shouldParse`
                    ClassSelector "test-class" 

        it "should fail on ill named selector" $
                selector `shouldFailOn` (".-class" :: ByteString)

        it "should parse type selector" $
            ("div" :: ByteString)
                ~> selector `shouldParse`
                    TypeSelector "div" 

        it "should parse id selector" $
            ("#test_id" :: ByteString)
                ~> selector `shouldParse`
                    IdSelector "test_id" 

        it "should parse attribute selector" $
            ("[href*=\"example\"]" :: ByteString)
                ~> selector `shouldParse`
                    AttributeSelector "href*=\"example\"" 

        it "should parse universal selector" $
            ("*" :: ByteString)
                ~> selector `shouldParse`
                    UniversalSelector 

        it "should parse pseudo class selector" $
            (":nth-child(1)" :: ByteString)
                ~> selector `shouldParse`
                    PseudoClass "nth-child()" 

        it "should parse pseudo element selector" $
            ("::-moz-progress-bar" :: ByteString)
                ~> selector `shouldParse`
                    PseudoElement "-moz-progress-bar" 
