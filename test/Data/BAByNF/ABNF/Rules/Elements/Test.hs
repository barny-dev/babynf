module Data.BAByNF.ABNF.Rules.Elements.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.PrettyPrint
import Data.BAByNF.ABNF.Rules.Elements qualified as Elements
import Data.BAByNF.ABNF.Model qualified as Model

moduleUnderTest :: String
moduleUnderTest = "Test-Data.BAByNF.ABNF.Rules.Elements"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    , testParseIntoModel
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    prettyPrint Elements.rule @?= "elements = alternation *c-wsp"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "opt1 / opt2-1 opt2-1"
    ,  "opt1 / opt2-1 opt2-1 ; comment\r\n "
    , "some-stuff ; comment\r\n "
    ] <&> \s -> HUnit.testCase (show s) $ 
        either
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules Elements.ref (stringAsBytesUnsafe s))

testParseIntoModel :: Tasty.TestTree
testParseIntoModel = Tasty.testGroup "parseIntoModel" $
    [ ("one / other", Model.Elements (Model.Alternation 
        [ Model.Concatenation [Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "one")))]
        , Model.Concatenation [Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "other")))]
        ]))
    , ("one / other   ; this is a comment\r\n ", Model.Elements (Model.Alternation 
        [ Model.Concatenation [Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "one")))]
        , Model.Concatenation [Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "other")))]
        ]))
    ] <&> \(s, o) -> HUnit.testCase (show s) $
        do
            tree <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
                return
                (parse rules Elements.ref (stringAsBytesUnsafe s))
            subTree <- case Tree.asSingleton tree >>= Tree.getSubtreeIfRef Elements.ref of
                Nothing -> HUnit.assertFailure $ "expected Elements ref node but result is tree with [" ++ show (length (Tree.nodes tree))  ++ "] children."
                Just subTree -> return subTree
            model <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string into model with error: [" ++ msg ++ "]")
                return
                (Elements.fromTree subTree)
            model @?= o