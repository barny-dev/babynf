module Data.BAByNF.ABNF.Rules.DefinedAs.Test where

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
import Data.BAByNF.ABNF.Rules.DefinedAs qualified as DefinedAs

moduleUnderTest :: String
moduleUnderTest = "Test-Data.BAByNF.ABNF.Rules.DefinedAs"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    , testParseIntoModel
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    prettyPrint DefinedAs.rule @?= "defined-as = *c-wsp (\"=\" / \"=/\") *c-wsp"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "="
    , "=/"
    , " = "
    , " =/ "
    , "; comment\r\n = "
    , "; comment\r\n =/ "
    , "; comment\r\n = ; comment \r\n "
    , "; comment\r\n =/ ; comment \r\n "
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules DefinedAs.ref (stringAsBytesUnsafe s))

testParseIntoModel :: Tasty.TestTree
testParseIntoModel = Tasty.testGroup "parseIntoModel" $
    [ ("=", Model.BasicDefinition)
    , ("=/", Model.IncrementalAlternative)
    , (" = ", Model.BasicDefinition)
    , (" =/ ", Model.IncrementalAlternative)
    ] <&> \(s, o) -> HUnit.testCase (show s) $
        do 
            tree <- either 
                (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]") 
                return 
                (parse rules DefinedAs.ref (stringAsBytesUnsafe s))
            subTree <- case Tree.asSingleton tree >>= Tree.getSubtreeIfRef DefinedAs.ref of
                Nothing -> HUnit.assertFailure $ "expected DefinedAs ref node but result is tree with [" ++ show (length (Tree.nodes tree))  ++ "] children."
                Just subTree -> return subTree
            model <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string into model with error: [" ++ msg ++ "]") 
                return
                (DefinedAs.fromTree subTree)
            model @?= o