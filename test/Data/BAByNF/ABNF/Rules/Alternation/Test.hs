module Data.BAByNF.ABNF.Rules.Alternation.Test where

import Data.Functor ((<&>))
import Data.List qualified as List

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Model qualified as Model
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.Alternation qualified as Alternation

moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.Alternation"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    , testParseIntoModel
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint Alternation.rule @?= "alternation = concatenation *(*c-wsp \"/\" *c-wsp concatenation)"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "rule / \"string\""
    , "rule"
    , "\"string\""
    , "rule1 / rule2"
    , "rule1/rule2"
    , "rule1   / rule2     / rule3"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules Alternation.ref (stringAsBytesUnsafe s))

testParseIntoModel :: Tasty.TestTree
testParseIntoModel = Tasty.testGroup "parseIntoModel" $
    [ ("someRule", Model.Alternation . List.singleton . Model.Concatenation . List.singleton $ Model.Repetition Model.NoRepeat (Model.RulenameElement . Model.Rulename $ stringAsBytesUnsafe "someRule"))
    , ("thisRule / thatRule / anotherRule andAfterThatToo", 
        Model.Alternation 
            [ Model.Concatenation . List.singleton $ Model.Repetition Model.NoRepeat (Model.RulenameElement . Model.Rulename $ stringAsBytesUnsafe "thisRule")
            , Model.Concatenation . List.singleton $ Model.Repetition Model.NoRepeat (Model.RulenameElement . Model.Rulename $ stringAsBytesUnsafe "thatRule")
            , Model.Concatenation
                [ Model.Repetition Model.NoRepeat (Model.RulenameElement . Model.Rulename $ stringAsBytesUnsafe "anotherRule")
                , Model.Repetition Model.NoRepeat (Model.RulenameElement . Model.Rulename $ stringAsBytesUnsafe "andAfterThatToo")
                ]
            ]
      )
    ] <&> \(s, o) -> HUnit.testCase (show s) $
        do 
            tree <- either 
                (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]") 
                return 
                (parse rules Alternation.ref (stringAsBytesUnsafe s))
            subTree <- case Tree.asSingleton tree >>= Tree.getSubtreeIfRef Alternation.ref of
                Nothing -> HUnit.assertFailure $ "expected Alternation ref node but result is tree with [" ++ show (length (Tree.nodes tree))  ++ "] children."
                Just subTree -> return subTree
            model <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string into model with error: [" ++ msg ++ "]") 
                return
                (Alternation.fromTree subTree)
            model @?= o