module Data.BAByNF.Core.Parseable.Test (testModule) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.ByteString qualified as BS
import Data.Attoparsec.ByteString qualified as A
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Data.BAByNF.ABNF.Model qualified as M
import Data.BAByNF.Core.Parseable (Parseable (..), Dict, toParser)
import Data.BAByNF.Core.RefDict (RefDict (RefDict))
import Data.BAByNF.Core.Repeat qualified as R
import Data.BAByNF.Core.Tree (Tree (Tree), Node (StringNode, RefNode))
import Data.BAByNF.Core.Tree qualified as T
import ParserFixtures (bytes, ref, literal)

testModule :: TestTree
testModule = testGroup "core interpreter contract"
    [ testCase "sequence preserves terminal fragments" $
        parses (seqP [literal "ab", literal "c"]) "abc" (leaves ["ab", "c"])
    , testCase "failed suffix retries an earlier successful alternative" $
        parses (seqP [altP [literal "a", literal "ab"], literal "c"])
            "abc" (leaves ["ab", "c"])
    , testCase "end of input can select a longer alternative" $
        parses (altP [literal "a", literal "ab"]) "ab" (leaves ["ab"])
    , testCase "failed branch restores input and discards its tree" $
        parses (altP [seqP [literal "ab", literal "x"], literal "abc"])
            "abc" (leaves ["abc"])
    , testCase "greedy repetition gives input back to its suffix" $
        parses (seqP [Rep (literal "a") R.zeroOrMore, literal "a"])
            "aaa" (leaves ["a", "a", "a"])
    , testCase "optional match can be abandoned for its suffix" $
        parses (seqP [Rep (literal "a") R.maybeOnce, literal "a"])
            "a" (leaves ["a"])
    , testCase "bounded repetition accepts its lower bound" $
        parses (Rep (literal "a") (R.from 1 (Just 1))) "a" (leaves ["a"])
    , testCase "bounded repetition accepts its upper bound" $
        parses (Rep (literal "a") (R.from 1 (Just 1))) "aa" (leaves ["a", "a"])
    , testCase "bounded repetition rejects too few matches" $
        rejects (Rep (literal "a") (R.exactly 2)) "a"
    , testCase "bounded repetition rejects too many matches" $
        rejects (Rep (literal "a") (R.upTo 2)) "aaa"
    , testCase "zero repetitions do not execute the unit" $
        parses (Rep (Unit "must not run" (fail "executed")) (R.exactly 0)) "" (Tree [])
    , testCase "finite repetition of an empty match terminates" $
        parses (Rep (literal "") (R.exactly 3)) "" (leaves ["", "", ""])
    , testCase "rule references create named nested nodes" $
        let rs = RefDict [(ref "outer", Rule (ref "inner")), (ref "inner", literal "a")]
         in run rs (Rule (ref "outer")) "a" @?=
            Right (Tree [RefNode (ref "outer") (Tree [RefNode (ref "inner") (leaves ["a"])])])
    , testCase "rule lookup is case insensitive" $
        run (RefDict [(ref "Token", literal "a")]) (Rule (ref "TOKEN")) "a" @?=
            Right (Tree [RefNode (ref "TOKEN") (leaves ["a"])])
    , testCase "undefined entry fails" $
        case run empty (Rule (ref "missing")) "" of
            Left _ -> pure ()
            Right t -> assertFailure ("unexpected tree: " ++ show t)
    , testCase "a consuming recursive rule terminates" $
        let rs = RefDict [(ref "r", altP [seqP [literal "a", Rule (ref "r")], literal ""])]
         in case run rs (Rule (ref "r")) "aaaa" of
            Left err -> assertFailure err
            Right t -> T.stringify t @?= bytes "aaaa"
    , testCase "custom unit output is preserved" $
        let p = Unit "custom" (A.word8 97 >> pure (Tree [RefNode (ref "custom") (leaves ["a"])]))
         in parses p "a" (Tree [RefNode (ref "custom") (leaves ["a"])])
    , testCase "trailing input is rejected" $ rejects (literal "a") "ab"
    , testCase "unexpected end of input is rejected" $ rejects (literal "ab") "a"
    , testCase "incremental input agrees with complete input across every split" $
        let p = toParser empty (seqP [altP [literal "a", literal "ab"], literal "c"])
            input = bytes "abc"
         in mapM_ (\n -> finish (A.feed (A.parse p (BS.take n input)) (BS.drop n input))
                    @?= Right (leaves ["ab", "c"])) [0 .. BS.length input]
    , testCase "alternative rollback spans multiple incremental chunks" $
        let p = toParser empty (seqP [altP [literal "a", literal "ab"], literal "c"])
            result = foldl A.feed (A.parse p (bytes "a")) [bytes "b", bytes "c", BS.empty]
         in finish result @?= Right (leaves ["ab", "c"])
    ]

empty :: Dict M.Rulename
empty = RefDict []

run :: Dict M.Rulename -> Parseable M.Rulename -> String -> Either String (Tree M.Rulename)
run rs p = A.parseOnly (toParser rs p) . bytes

parses :: Parseable M.Rulename -> String -> Tree M.Rulename -> Assertion
parses p input expected = run empty p input @?= Right expected

rejects :: Parseable M.Rulename -> String -> Assertion
rejects p input = case run empty p input of
    Left _ -> pure ()
    Right t -> assertFailure ("unexpected tree: " ++ show t)

leaves :: [String] -> Tree M.Rulename
leaves = Tree . map (StringNode . bytes)

seqP, altP :: [Parseable M.Rulename] -> Parseable M.Rulename
seqP (p:ps) = Seq (p :| ps)
seqP [] = error "empty sequence fixture"
altP (p:ps) = Alt (p :| ps)
altP [] = error "empty alternative fixture"

finish :: A.Result a -> Either String a
finish (A.Done rest value)
    | BS.null rest = Right value
    | otherwise = Left "unconsumed input"
finish (A.Fail _ _ err) = Left err
finish (A.Partial k) = case k BS.empty of
    A.Partial _ -> Left "still partial after EOF"
    result -> finish result
