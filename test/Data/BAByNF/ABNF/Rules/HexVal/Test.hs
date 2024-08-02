module Data.BAByNF.ABNF.Rules.HexVal.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)
import Data.BAByNF.Util.Hex (Seq (..), Digit (..))

import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Model qualified as Model 
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.HexVal qualified as HexVal

moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.HexVal"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    , testParseIntoModel
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint HexVal.rule @?= "hex-val = \"x\" 1*HEXDIG [1*(\".\" 1*HEXDIG) / (\"-\" 1*HEXDIG)]"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "x0"
    , "xF"
    , "xFF"
    , "x01-FF"
    , "x01.02.B1.F0"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules HexVal.ref (stringAsBytesUnsafe s))

testParseIntoModel :: Tasty.TestTree
testParseIntoModel = Tasty.testGroup "parseIntoModel" $
    [ ("xBF", Model.SeqHexVal [Seq [XB, XF]])
    , ("x01.C2.F0", 
        Model.SeqHexVal [ Seq [X0, X1]
                        , Seq [XC, X2]
                        , Seq [XF, X0]
                        ]
      )
    , ("x01-FF", 
        Model.RangeHexVal 
            (Seq [X0, X1]) 
            (Seq [XF, XF])
      )
    ] <&> \(s, o) -> HUnit.testCase (show s) $
        do 
            tree <- either 
                (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]") 
                return 
                (parse rules HexVal.ref (stringAsBytesUnsafe s))
            subTree <- case Tree.asSingleton tree >>= Tree.getSubtreeIfRef HexVal.ref of
                Nothing -> HUnit.assertFailure $ "expected HexVal ref node but result is tree with [" ++ show (length (Tree.nodes tree))  ++ "] children."
                Just subTree -> return subTree
            model <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string into model with error: [" ++ msg ++ "]") 
                return
                (HexVal.fromTree subTree)
            model @?= o