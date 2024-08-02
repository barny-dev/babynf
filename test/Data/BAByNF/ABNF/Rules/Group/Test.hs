module Data.BAByNF.ABNF.Rules.Group.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.PrettyPrint
import Data.BAByNF.ABNF.Rules.Group qualified as Group
import Data.BAByNF.ABNF.Model qualified as Model

moduleUnderTest :: String
moduleUnderTest = "Test-Data.BAByNF.ABNF.Rules.Group"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    , testParseIntoModel
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    prettyPrint Group.rule @?= "group = \"(\" *c-wsp alternation *c-wsp \")\""

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "( opt1 / opt2 / opt3 )"
    , "( opt1 ; comment1\r\n opt2 ; comment2\r\n opt3)"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules Group.ref (stringAsBytesUnsafe s))

testParseIntoModel :: Tasty.TestTree
testParseIntoModel = Tasty.testGroup "parseIntoModel" $
    [ ("(one / other)", Model.Group (Model.Alternation 
        [ Model.Concatenation [ Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "one")))]
        , Model.Concatenation [Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "other")))]
        ]))
    ] <&> \(s, o) -> HUnit.testCase (show s) $
        do
            tree <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
                return
                (parse rules Group.ref (stringAsBytesUnsafe s))
            subTree <- case Tree.asSingleton tree >>= Tree.getSubtreeIfRef Group.ref of
                Nothing -> HUnit.assertFailure $ "expected Group ref node but result is tree with [" ++ show (length (Tree.nodes tree))  ++ "] children."
                Just subTree -> return subTree
            model <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string into model with error: [" ++ msg ++ "]")
                return
                (Group.fromTree subTree)
            model @?= o