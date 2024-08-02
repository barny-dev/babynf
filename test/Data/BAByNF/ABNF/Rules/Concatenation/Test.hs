module Data.BAByNF.ABNF.Rules.Concatenation.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF.Model qualified as Model
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.PrettyPrint
import Data.BAByNF.ABNF.Rules.Concatenation qualified as Concatenation

moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.Concatenation"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    , testParseIntoModel
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    prettyPrint Concatenation.rule @?= "concatenation = repetition *(1*c-wsp repetition)"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "rule1 rule2"
    , "just-this-role"
    , "1*these-rules"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules Concatenation.ref (stringAsBytesUnsafe s))

testParseIntoModel :: Tasty.TestTree
testParseIntoModel = Tasty.testGroup "parseIntoModel" $
    [ ("rule1 rule2", Model.Concatenation
                [ Model.Repetition Model.NoRepeat (Model.RulenameElement . Model.Rulename $ stringAsBytesUnsafe "rule1")
                , Model.Repetition Model.NoRepeat (Model.RulenameElement . Model.Rulename $ stringAsBytesUnsafe "rule2")
                ])
    ] <&> \(s, o) -> HUnit.testCase (show s) $
        do 
            tree <- either 
                (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]") 
                return 
                (parse rules Concatenation.ref (stringAsBytesUnsafe s))
            subTree <- case Tree.asSingleton tree >>= Tree.getSubtreeIfRef Concatenation.ref of
                Nothing -> HUnit.assertFailure $ "expected Concatenation ref node but result is tree with [" ++ show (length (Tree.nodes tree))  ++ "] children."
                Just subTree -> return subTree
            model <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string into model with error: [" ++ msg ++ "]") 
                return
                (Concatenation.fromTree subTree)
            model @?= o
