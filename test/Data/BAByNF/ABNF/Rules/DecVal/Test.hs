module Data.BAByNF.ABNF.Rules.DecVal.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)
import Data.BAByNF.Util.Decimal (Seq(..), Digit(..))

import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF.Model qualified as Model
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.PrettyPrint
import Data.BAByNF.ABNF.Rules.DecVal qualified as DecVal

moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.DecVal"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    , testParseIntoModel
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    prettyPrint DecVal.rule @?= "dec-val = \"d\" 1*DIGIT [1*(\".\" 1*DIGIT) / (\"-\" 1*DIGIT)]"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "d0"
    , "d9"
    , "d255"
    , "d001-255"
    , "d001.002.090.128"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules DecVal.ref (stringAsBytesUnsafe s))

testParseIntoModel :: Tasty.TestTree
testParseIntoModel = Tasty.testGroup "parseIntoModel" $
    [ ("d128", Model.SeqDecVal [Seq [D1, D2, D8]])
    , ("d098.128.240", 
        Model.SeqDecVal [ Seq [D0, D9, D8]
                        , Seq [D1, D2, D8]
                        , Seq [D2, D4, D0]
                        ]
      )
    , ("d001-255", 
        Model.RangeDecVal 
            (Seq [D0, D0, D1]) 
            (Seq [D2, D5, D5])
      )
    ] <&> \(s, o) -> HUnit.testCase (show s) $
        do 
            tree <- either 
                (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]") 
                return 
                (parse rules DecVal.ref (stringAsBytesUnsafe s))
            subTree <- case Tree.asSingleton tree >>= Tree.getSubtreeIfRef DecVal.ref of
                Nothing -> HUnit.assertFailure $ "expected DecVal ref node but result is tree with [" ++ show (length (Tree.nodes tree))  ++ "] children."
                Just subTree -> return subTree
            model <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string into model with error: [" ++ msg ++ "]") 
                return
                (DecVal.fromTree subTree)
            model @?= o