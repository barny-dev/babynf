module Data.BAByNF.ABNF.Rules.Repetition.Test where

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
import Data.BAByNF.ABNF.Rules.Repetition qualified as Repetition


moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.Repetition"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    , testParseIntoModel
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    prettyPrint Repetition.rule @?= "repetition = [repeat] element"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "ground-hog"
    , "2ground-hog"
    , "1*ground-hog"
    , "*ground-hog"
    , "*2ground-hog"
    , "*%xFF"
    , "*(some group)"
    , "*\"some text\""
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules Repetition.ref (stringAsBytesUnsafe s))

testParseIntoModel :: Tasty.TestTree
testParseIntoModel = Tasty.testGroup "parseIntoModel" $
    [ ("ground-hog", Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "ground-hog"))))
    , ("2ground-hog", Model.Repetition (Model.FixedRepeat 2) (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "ground-hog"))))
    ] <&> \(s, o) -> HUnit.testCase (show s) $
        do 
            tree <- either 
                (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]") 
                return 
                (parse rules Repetition.ref (stringAsBytesUnsafe s))
            subTree <- case Tree.asSingleton tree >>= Tree.getSubtreeIfRef Repetition.ref of
                Nothing -> HUnit.assertFailure $ "expected Repetition ref node but result is tree with [" ++ show (length (Tree.nodes tree))  ++ "] children."
                Just subTree -> return subTree
            model <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string into model with error: [" ++ msg ++ "]") 
                return
                (Repetition.fromTree subTree)
            model @?= o