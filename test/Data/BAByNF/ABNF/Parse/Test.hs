module Data.BAByNF.ABNF.Parse.Test
    ( testModule
    ) where

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Core.Tree (Tree (..))
import Data.BAByNF.Core.Ref qualified as Ref
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.ABNF.Parse (parseRulelist, parse)
import Data.Either (isRight)
import Data.BAByNF.ABNF.Model qualified as Model
import Data.BAByNF.ABNF.Core qualified as Core

moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Parse"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest
    [ testParseRulelist ]

testParseRulelist :: Tasty.TestTree
testParseRulelist = HUnit.testCase "parseRulelist" $
    case (
        parseRulelist rules
        >>=
        (\rulelist -> parse (withCore rulelist) (Model.Rulename (Ascii.stringAsBytesUnsafe "ehlo-ok-rsp")) ehloResponse)
    ) of 
      Left err -> HUnit.assertFailure $ "Expected fragment to be successfully parsed but failed with error message "  ++ err
      Right tree ->
        expectRefTreeThat (ref "ehlo-ok-rsp") tree $ \nodes -> 
          return nodes >>=
          expectString "250" >>=
          expectRefNodeLike Core.spRef " " >>=
          expectRefNodeLike (ref "Domain") "mail.server.org" >>=
          expectRefNodeLike Core.spRef " " >>=
          expectRefNodeLike (ref "ehlo-greet") "greetings fellow traveller!" >>=
          expectRefNodeLike Core.crlfRef "\r\n" >>=
          expectEnd
    -- todo: node list assert (monoid?)
    where ehloResponse = Ascii.stringAsBytesUnsafe "250 mail.server.org greetings fellow traveller!\r\n"
          rules = Ascii.stringAsBytesUnsafe
            "ehlo-ok-rsp  = ( \"250\" SP Domain [ SP ehlo-greet ] CRLF )\r\n\
            \             / ( \"250-\" Domain [ SP ehlo-greet ] CRLF\r\n\
            \                 *( \"250-\" ehlo-line CRLF )\r\n\
            \               \"250\" SP ehlo-line CRLF )\r\n\
            \Domain = \"mail.server.org\"\r\n\
            \ehlo-greet = 1*(%d0-9 / %d11-12 / %d14-127)\r\n\
            \ehlo-line = ehlo-keyword *( SP ehlo-param )\r\n\
            \ehlo-keyword = (ALPHA / DIGIT) *(ALPHA / DIGIT / \"-\")\r\n\
            \ehlo-param = 1*(%d33-126)\r\n"
          withCore (Model.Rulelist rules) = Model.Rulelist (rules ++ Core.rules)
          expectString s l =
            let expected = stringNode s
             in case l of
                [] -> HUnit.assertFailure $ "Expected " ++ show expected ++ " but no more elements found"
                x:xs -> (x @?= expected) >> return xs
          expectRefNodeLike r s l =
            let expected = refNodeWithString r s
             in case l of
                [] -> HUnit.assertFailure $ "Expected node like " ++ show expected ++ " but no more elements found"
                x:xs -> case x of
                    Tree.RefNode r' t -> (Tree.RefNode r' (Tree [Tree.StringNode $ Tree.stringify t]) @?= expected) >> return xs
                    t@(Tree.StringNode _) -> HUnit.assertFailure $ "Expected node like " ++ show expected ++ " but found " ++ show t
          expectEnd l =
            case l of
                [] -> return ()
                _ -> HUnit.assertFailure $ "Expected end of node list but " ++ show (length l) ++ " more elements found"
          stringNode s = Tree.StringNode (Ascii.stringAsBytesUnsafe s) :: Tree.Node Model.Rulename
          refNodeWithString r s = Tree.RefNode r (Tree [stringNode s])
          ref = Model.Rulename . Ascii.stringAsBytesUnsafe
          expectRefTreeThat r t a = case Tree.nodes t of
            [Tree.RefNode r' t'] | Ref.eq r r' -> a (Tree.nodes t')
                                 | otherwise -> HUnit.assertFailure $ "Expected tree of ref " ++ show r ++ " but found " ++ show r'
            x -> HUnit.assertFailure $ "Expected tree of ref " ++ show r ++ " but found " ++ show (length x) ++ " elements"    

