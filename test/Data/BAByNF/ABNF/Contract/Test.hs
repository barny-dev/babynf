module Data.BAByNF.ABNF.Contract.Test (testModule) where

import Data.ByteString qualified as BS
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Data.BAByNF.ABNF.Parse qualified as ABNF
import Data.BAByNF.Core.Tree qualified as T
import ParserFixtures

testModule :: TestTree
testModule = testGroup "ABNF integration contract"
    [ testCase "HTTP request fixture" $ matches httpGrammar httpRequest
    , testCase "SMTP multiline fixture" $ matches smtpGrammar smtpResponse
    , testCase "grammar comments are recognized and omitted from the model" $
        case ABNF.parseRulelist (bytes "; heading\r\nstart = \"a\" ; inline\r\n") of
            Left err -> assertFailure err
            Right rs -> ABNF.parseRulelist (bytes "start = \"a\"\r\n") @?= Right rs
    , testCase "incremental rule definitions append alternatives" $
        matches (grammar ["start = token", "token = \"a\"", "TOKEN =/ \"b\""]) (bytes "b")
    , testCase "default literals ignore ASCII case" $
        matches (grammar ["start = \"AbC\""]) (bytes "aBc")
    , testCase "explicit sensitive literals reject a different case" $
        rejects (parseStart (grammar ["start = %s\"AbC\""]) (bytes "abc"))
    , testCase "numeric terminals preserve NUL and 255" $
        matches (grammar ["start = %d0 %xFF"]) (BS.pack [0, 255])
    , testCase "unsupported prose fails when reached" $
        rejects (parseStart (grammar ["start = <custom definition>"]) (bytes "anything"))
    , testCase "syntax reader consumes all input" $
        rejects (ABNF.parseRulelist (bytes "start = \"a\"\r\n@"))
    , testGroup "malformed grammar text is rejected"
        [ testCase (show input) $ rejects (ABNF.parseRulelist (bytes input))
        | input <- [ "", "start : \"a\"\r\n", "start = \"unterminated\r\n"
                   , "start = (\"a\"\r\n", "start = %xGG\r\n", "start = \"a\"" ]
        ]
    ]
    where matches rs input = case parseStart rs input of
            Left err -> assertFailure err
            Right t -> T.stringify t @?= input
          rejects result = case result of
            Left _ -> pure ()
            Right _ -> assertFailure "unexpectedly accepted input"
