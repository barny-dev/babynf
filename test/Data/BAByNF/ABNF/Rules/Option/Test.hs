module Data.BAByNF.ABNF.Rules.Option.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Model qualified as Model
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.Option qualified as Option


moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.Option"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    , testParseIntoModel
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint Option.rule @?= "option = \"[\" *c-wsp alternation *c-wsp \"]\""

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "[ref]"
    , "[alt1 / alt2-1 alt2-2]"
    , "[this that]"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules Option.ref (stringAsBytesUnsafe s))

testParseIntoModel :: Tasty.TestTree
testParseIntoModel = Tasty.testGroup "parseIntoModel" $
    [ ("[one / other]", Model.Option (Model.Alternation 
        [ Model.Concatenation [ Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "one")))]
        , Model.Concatenation [Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "other")))]
        ]))
    ] <&> \(s, o) -> HUnit.testCase (show s) $
        do
            tree <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
                return
                (parse rules Option.ref (stringAsBytesUnsafe s))
            subTree <- case Tree.asSingleton tree >>= Tree.getSubtreeIfRef Option.ref of
                Nothing -> HUnit.assertFailure $ "expected Option ref node but result is tree with [" ++ show (length (Tree.nodes tree))  ++ "] children."
                Just subTree -> return subTree
            model <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string into model with error: [" ++ msg ++ "]")
                return
                (Option.fromTree subTree)
            model @?= o