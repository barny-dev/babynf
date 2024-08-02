module Data.BAByNF.ABNF.Rules.BinVal.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)
import Data.BAByNF.Util.Binary (Seq (..), Digit (..))

import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF.Model qualified as Model 
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.PrettyPrint
import Data.BAByNF.ABNF.Rules.BinVal qualified as BinVal


moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.BinVal"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    , testParseIntoModel
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    prettyPrint BinVal.rule @?= "bin-val = \"b\" 1*BIT [1*(\".\" 1*BIT) / (\"-\" 1*BIT)]"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "b0"
    , "b1"
    , "b01010101"
    , "b00001111-11110000"
    , "b00000001.00000010.00000100"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules BinVal.ref (stringAsBytesUnsafe s))

testParseIntoModel :: Tasty.TestTree
testParseIntoModel = Tasty.testGroup "parseIntoModel" $
    [ ("b00001111", Model.SeqBinVal [Seq [B0, B0, B0, B0, B1, B1, B1, B1]])
    , ("b00001111.00010000.00010100", 
        Model.SeqBinVal [ Seq [B0, B0, B0, B0, B1, B1, B1, B1]
                        , Seq [B0, B0, B0, B1, B0, B0, B0, B0]
                        , Seq [B0, B0, B0, B1, B0, B1, B0, B0]
                        ]
      )
    , ("b00000100-00000111", 
        Model.RangeBinVal 
            (Seq [B0, B0, B0, B0, B0, B1, B0, B0]) 
            (Seq [B0, B0, B0, B0, B0, B1, B1, B1])
      )
    ] <&> \(s, o) -> HUnit.testCase (show s) $
        do 
            tree <- either 
                (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]") 
                return 
                (parse rules BinVal.ref (stringAsBytesUnsafe s))
            subTree <- case Tree.asSingleton tree >>= Tree.getSubtreeIfRef BinVal.ref of
                Nothing -> HUnit.assertFailure $ "expected BinVal ref node but result is tree with [" ++ show (length (Tree.nodes tree))  ++ "] children."
                Just subTree -> return subTree
            model <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string into model with error: [" ++ msg ++ "]") 
                return
                (BinVal.fromTree subTree)
            model @?= o