module Data.BAByNF.ABNF.Rules.Repeat.Test where

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
import Data.BAByNF.ABNF.Rules.Repeat qualified as Repeat


moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.Repeat"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    , testParseIntoModel
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint Repeat.rule @?= "repeat = 1*DIGIT / (*DIGIT \"*\" *DIGIT)"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "2"
    , "15"
    , "*"
    , "15*"
    , "*8"
    , "1*4"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules Repeat.ref (stringAsBytesUnsafe s))

testParseIntoModel :: Tasty.TestTree
testParseIntoModel = Tasty.testGroup "parseIntoModel" $
    [ ("2", Model.FixedRepeat 2)
    , ("15*", Model.RangedRepeat (Model.FixedBound 15) Model.UnBound)
    , ("*8", Model.RangedRepeat Model.UnBound (Model.FixedBound 8))
    , ("1*4", Model.RangedRepeat (Model.FixedBound 1) (Model.FixedBound 4))
    , ("*", Model.RangedRepeat Model.UnBound Model.UnBound)
    ] <&> \(s, o) -> HUnit.testCase (show s) $
        do 
            tree <- either 
                (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]") 
                return 
                (parse rules Repeat.ref (stringAsBytesUnsafe s))
            subTree <- case Tree.asSingleton tree >>= Tree.getSubtreeIfRef Repeat.ref of
                Nothing -> HUnit.assertFailure $ "expected Repeat ref node but result is tree with [" ++ show (length (Tree.nodes tree))  ++ "] children."
                Just subTree -> return subTree
            model <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string into model with error: [" ++ msg ++ "]") 
                return
                (Repeat.fromTree subTree)
            model @?= o
