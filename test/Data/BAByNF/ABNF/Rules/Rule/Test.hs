module Data.BAByNF.ABNF.Rules.Rule.Test where

import Data.Functor ((<&>))
import Data.List qualified as List

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.PrettyPrint
import Data.BAByNF.ABNF.Rules.Rule qualified as Rule
import Data.BAByNF.ABNF.Model qualified as Model


moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.Rule"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    , testParseIntoModel
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    prettyPrint Rule.rule @?= "rule = rulename defined-as elements c-nl"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "rule = rulename defined-as elements c-nl\r\n"
    , "rule =  rulename defined-as elements c-nl; continues if next line starts\r\n"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules Rule.ref (stringAsBytesUnsafe s))

testParseIntoModel :: Tasty.TestTree
testParseIntoModel = Tasty.testGroup "parseIntoModel" $
    [ ("rule = rulename defined-as elements c-nl\r\n", Model.Rule 
        (Model.Rulename (stringAsBytesUnsafe "rule")) Model.BasicDefinition 
            ( Model.Elements
            . Model.Alternation
            . List.singleton
            . Model.Concatenation
            $ [ Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "rulename")))
              , Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "defined-as")))
              , Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "elements")))
              , Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "c-nl")))
              ])
      ) 
    ] <&> \(s, o) -> HUnit.testCase (show s) $
        do
            tree <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
                return
                (parse rules Rule.ref (stringAsBytesUnsafe s))
            subTree <- case Tree.asSingleton tree >>= Tree.getSubtreeIfRef Rule.ref of
                Nothing -> HUnit.assertFailure $ "expected Rule ref node but result is tree with [" ++ show (length (Tree.nodes tree))  ++ "] children."
                Just subTree -> return subTree
            model <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string into model with error: [" ++ msg ++ "]")
                return
                (Rule.fromTree subTree)
            model @?= o