{-# LANGUAGE OverloadedStrings #-}

module CssDocumentSpec where

import           CssDocument
import           Data.Text
import           Selector              (Selector (..))
import           Test.Hspec
import           Test.Hspec.Attoparsec


spec :: Spec
spec = do
    describe "parse declaration headers" $ do
        it "should parse class names joined together" $
            (".test-class.test-class2" :: Text)
                ~> declarationHeader `shouldParse`
                    [ ClassSelector "test-class"
                    , ClassSelector "test-class2"
                    ]

        it "should parse type and attribute selector" $
            ("a[href*=\"example\"]" :: Text)
                ~> declarationHeader `shouldParse`
                    [ TypeSelector "a"
                    , AttributeSelector "href*=\"example\"" ]

        it "should parse orphaned pseudo class" $
            (":checked" :: Text)
                ~> declarationHeader `shouldParse`
                    [ PseudoClass "checked" ]

        it "should parse class names separated by whitespace" $
            (".test-class ul li" :: Text)
                ~> declarationHeader `shouldParse`
                [ ClassSelector "test-class"
                , TypeSelector "ul"
                , TypeSelector "li"
                ]

        it "should parse class names separated by comma" $
            (".test-class, .test-class2" :: Text)
                ~> declarationHeader `shouldParse`
                    [ ClassSelector "test-class"
                    , ClassSelector "test-class2"
                    ]

        it "should parse class names separated by >" $
            (".test-class > .test-class2" :: Text)
                ~> declarationHeader `shouldParse`
                    [ ClassSelector "test-class"
                    , ClassSelector "test-class2"
                    ]

        it "should parse class names separated by ~" $
            (".test-class ~ .test-class2" :: Text)
                ~> declarationHeader `shouldParse`
                    [ ClassSelector "test-class"
                    , ClassSelector "test-class2"
                    ]

        it "should parse class names separated by +" $
            (".test-class + .test-class2" :: Text)
                ~> declarationHeader `shouldParse`
                    [ ClassSelector "test-class"
                    , ClassSelector "test-class2"
                    ]

        it "should parse class name with pseudo class" $
            (".test-class:hover" :: Text)
                ~> declarationHeader `shouldParse`
                    [ ClassSelector "test-class"
                    , PseudoClass "hover"
                    ]

        it "should parse class name with pseudo element" $
            (".test-class::before" :: Text)
                ~> declarationHeader `shouldParse`
                    [ ClassSelector "test-class"
                    , PseudoElement "before"
                    ]

        it "should parse class names with pseudo classes between" $
            (".test-class::before, .test-class2" :: Text)
                ~> declarationHeader `shouldParse`
                    [ ClassSelector "test-class"
                    , PseudoElement "before"
                    , ClassSelector "test-class2"
                    ]

    describe "parse full declarations" $ do
        it "should parse class name" $
            (".test-class { width: 100px; }" :: Text)
                ~> declaration `shouldParse`
                    [ ClassSelector "test-class" ]

        it "should parse multiline css" $
            (".test-class {\n    width: 100px;\n}" :: Text)
                ~> declaration `shouldParse`
                    [ ClassSelector "test-class" ]

        it "should parse multiline css with multiple selectors" $
            ("[ng\\:cloak],\n [ng-cloak],\n [data-ng-cloak],\n [x-ng-cloak],\n .ng-cloak,\n .x-ng-cloak {\n display: none !important; }" :: Text)
                ~> declaration `shouldParse`
                    [ AttributeSelector "ng\\:cloak"
                    , AttributeSelector "ng-cloak"
                    , AttributeSelector "data-ng-cloak"
                    , AttributeSelector "x-ng-cloak"
                    , ClassSelector "ng-cloak"
                    , ClassSelector "x-ng-cloak"
                    ]

        it "should parse class name without whitespaces" $
            (".test-class{width: 100px;}" :: Text)
                ~> declaration `shouldParse`
                    [ ClassSelector "test-class"]

        it "should not care about anything inside a class declaration body" $
            (".test-class { background: \"...lookslikeaclass.butisnt\" }" :: Text)
                ~> declaration `shouldParse`
                    [ ClassSelector "test-class"]

    describe "parse document" $ do
        it "should parse multiple class names" $
            (".test-class { width: 100px; } .test-class2 { height: 100px; }" :: Text)
                ~> document `shouldParse`
                    [ ClassSelector "test-class"
                    , ClassSelector "test-class2"
                    ]

        it "should skip comments" $
            ("/* comment */ .test-class { width: 100px; }" :: Text)
                ~> document `shouldParse`
                    [ ClassSelector "test-class" ]

        it "should skip comments between classes" $
            (".test-class { width: 100px; } /* comment */ .test-class2 { height: 100px; }" :: Text)
                ~> document `shouldParse`
                    [ ClassSelector "test-class"
                    , ClassSelector "test-class2"
                    ]

        it "should skip comments with * in them" $
            ("/*** comment ***/ .test-class { width: 100px; }" :: Text)
                ~> document `shouldParse`
                    [ ClassSelector "test-class" ]

        it "should skip one line at-rule" $
            ("@charset \"UTF-8\"; .test-class { width: 100px; }" :: Text)
                ~> document `shouldParse`
                    [ ClassSelector "test-class" ]

        it "should parse selectors inside a nested at-rule" $
            ("@media screen { .u-media-print { display: none; } .footer { display: none; } }" :: Text)
                ~> document `shouldParse`
                    [ ClassSelector "u-media-print"
                    , ClassSelector "footer"
                    ]

        it "should parse selectors in sequence" $
            ("@media screen { .u-media-print { display: none; } } .footer { display: none; }" :: Text)
                ~> document `shouldParse`
                    [ ClassSelector "u-media-print"
                    , ClassSelector "footer"
                    ]
