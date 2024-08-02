module Data.BAByNF.ABNF.Rules.Rulelist.Test where

import Data.Functor ((<&>))
import Data.List qualified as List

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.PrettyPrint
import Data.BAByNF.ABNF.Rules.Rulelist qualified as Rulelist
import Data.BAByNF.ABNF.Model qualified as Model
import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)


moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.Rulelist"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    , testParseIntoModel
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    prettyPrint Rulelist.rule @?= "rulelist = 1*(rule / (*c-wsp c-nl))"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "rule = rulename defined-as elements c-nl\r\n"
    , "rule1 = %x02-0F\r\nrule2 = \"something else\"\r\n"
    , "\r\n"
    , "ruleX = <this> / <that>; some comment\r\nruleY = \"impossible\"\r\n"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules Rulelist.ref (stringAsBytesUnsafe s))

testParseIntoModel :: Tasty.TestTree
testParseIntoModel = Tasty.testGroup "parseIntoModel" $
    [ ("rule = rulename defined-as elements c-nl\r\n", Model.Rulelist [
        Model.Rule (Model.Rulename (stringAsBytesUnsafe "rule")) Model.BasicDefinition 
            ( Model.Elements
            . Model.Alternation
            . List.singleton
            . Model.Concatenation
            $ [ Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "rulename")))
              , Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "defined-as")))
              , Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "elements")))
              , Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "c-nl")))
              ]) 
      ])
    , ("ruleX = <this> / <that>; some comment\r\nruleY = \"impossible\"\r\n", Model.Rulelist [
        Model.Rule (Model.Rulename (stringAsBytesUnsafe "ruleX")) Model.BasicDefinition 
            ( Model.Elements
            . Model.Alternation
            $ [ Model.Concatenation . List.singleton $ Model.Repetition Model.NoRepeat (Model.ProseValElement (Model.ProseVal (stringAsBytesUnsafe "this")))
              , Model.Concatenation . List.singleton $ Model.Repetition Model.NoRepeat (Model.ProseValElement (Model.ProseVal (stringAsBytesUnsafe "that")))
              ]) 
        , Model.Rule (Model.Rulename (stringAsBytesUnsafe "ruleY")) Model.BasicDefinition 
            ( Model.Elements
            . Model.Alternation
            . List.singleton
            . Model.Concatenation
            . List.singleton
            $ Model.Repetition Model.NoRepeat (Model.CharValElement (Model.CaseInsensitiveCharVal (Model.CaseInsensitiveString (Model.QuotedString (stringAsBytesUnsafe "impossible")))))
            ) 
      ])
    ] <&> \(s, o) -> HUnit.testCase (show s) $
        do
            tree <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
                return
                (parse rules Rulelist.ref (stringAsBytesUnsafe s))
            subTree <- case Tree.asSingleton tree >>= Tree.getSubtreeIfRef Rulelist.ref of
                Nothing -> HUnit.assertFailure $ "expected Rulelist ref node but result is tree with [" ++ show (length (Tree.nodes tree))  ++ "] children."
                Just subTree -> return subTree
            model <- either
                (\msgs -> HUnit.assertFailure $ "failed to parse provided string into model with errors: [\n" ++ List.intercalate "\n" msgs ++ "\n]")
                return
                (Rulelist.fromTree subTree)
            model @?= o