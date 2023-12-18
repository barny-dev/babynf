module Data.BAByNF.Test where

import qualified Test.Tasty as T

import qualified Data.BAByNF.Util.Ascii.Test as BUAT
import qualified Data.BAByNF.ABNF.Grammar.Test as BGAT
import qualified Data.BAByNF.ABNF.Rules.Test as BGT
import qualified Data.BAByNF.Parseable.Test as BPT

tests :: T.TestTree
tests = "BAByNF tests" `T.testGroup`
    [ BUAT.tests
    , BGT.tests
    , BPT.tests
    , BGAT.tests
    ]
