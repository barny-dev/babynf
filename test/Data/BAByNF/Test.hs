module Data.BAByNF.Test where

import qualified Test.Tasty as T

import qualified Data.BAByNF.Util.Ascii.Test as BUAT
import qualified Data.BAByNF.Grammar.ABNF.Test as BGAT
import qualified Data.BAByNF.Grammar.Test as BGT
import qualified Data.BAByNF.Parsers.Test as BPT

tests :: T.TestTree
tests = "BAByNF tests" `T.testGroup`
    [ BUAT.tests
    , BGT.tests
    , BPT.tests
    , BGAT.tests
    ]
