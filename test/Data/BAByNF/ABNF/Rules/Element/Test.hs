module Data.BAByNF.ABNF.Rules.Element.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.Util.Hex qualified as Hex

import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.PrettyPrint
import Data.BAByNF.ABNF.Rules.Element qualified as Element
import Data.BAByNF.ABNF.Model qualified as Model


moduleUnderTest :: String
moduleUnderTest = "Test-Data.BAByNF.ABNF.Rules.Element"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest
    [ testPrettyPrint
    , testParse
    , testParseIntoModel
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    prettyPrint Element.rule @?= "element = rulename / group / option / char-val / num-val / prose-val"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "some-rule"
    , "\"some char value\""
    , "%i\"some other char value\""
    , "%s\"yet another char value\""
    , "[ something-optional ]"
    , "( alt1 / alt2-1 alt2-2 )"
    , "%xFF"
    , "%x00-FF"
    , "%x01.0A.0A.02.B1"
    , "%d255"
    , "%d000-255"
    , "%d001.010.010.002.177"
    , "%b11111111"
    , "%b00000000-11111111"
    , "%b00000001.00001010.10110001"
    , "<this is prose>"
    ] <&> \s -> HUnit.testCase (show s) $
        either
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ())
            (parse rules Element.ref (stringAsBytesUnsafe s))

testParseIntoModel :: Tasty.TestTree
testParseIntoModel = Tasty.testGroup "parseIntoModel" $
    [ ("some-ref", Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "some-ref")))
    , ("(one / other)", Model.GroupElement (Model.Group (Model.Alternation 
        [ Model.Concatenation [ Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "one")))]
        , Model.Concatenation [Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "other")))]
        ])))
    , ("[one / other]", Model.OptionElement (Model.Option (Model.Alternation 
        [ Model.Concatenation [ Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "one")))]
        , Model.Concatenation [Model.Repetition Model.NoRepeat (Model.RulenameElement (Model.Rulename (stringAsBytesUnsafe "other")))]
        ])))
    , ("\"some text\"", Model.CharValElement (Model.CaseInsensitiveCharVal (Model.CaseInsensitiveString (Model.QuotedString (stringAsBytesUnsafe "some text")))))
    , ("%xA0-FF", Model.NumValElement (Model.HexNumVal (Model.RangeHexVal (Hex.Seq [Hex.XA, Hex.X0]) (Hex.Seq [Hex.XF, Hex.XF]))))
    , ("<this is prose>", Model.ProseValElement (Model.ProseVal (stringAsBytesUnsafe "this is prose")))
    ] <&> \(s, o) -> HUnit.testCase (show s) $
        do
            tree <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
                return
                (parse rules Element.ref (stringAsBytesUnsafe s))
            subTree <- case Tree.asSingleton tree >>= Tree.getSubtreeIfRef Element.ref of
                Nothing -> HUnit.assertFailure $ "expected Element ref node but result is tree with [" ++ show (length (Tree.nodes tree))  ++ "] children."
                Just subTree -> return subTree
            model <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string into model with error: [" ++ msg ++ "]")
                return
                (Element.fromTree subTree)
            model @?= o