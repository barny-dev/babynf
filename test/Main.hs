module Main (main) where

import Test.Tasty qualified as Tasty

import Data.BAByNF.ABNF.Rules.Alternation.Test qualified as ABNF.Rules.Alternation
import Data.BAByNF.ABNF.Rules.BinVal.Test qualified as ABNF.Rules.BinVal
import Data.BAByNF.ABNF.Rules.CaseInsensitiveString.Test qualified as ABNF.Rules.CaseInsensitiveString
import Data.BAByNF.ABNF.Rules.CaseSensitiveString.Test qualified as ABNF.Rules.CaseSensitiveString
import Data.BAByNF.ABNF.Rules.CharVal.Test qualified as ABNF.Rules.CharVal
import Data.BAByNF.ABNF.Rules.CNl.Test qualified as ABNF.Rules.CNl
import Data.BAByNF.ABNF.Rules.Comment.Test qualified as ABNF.Rules.Comment
import Data.BAByNF.ABNF.Rules.CWsp.Test qualified as ABNF.Rules.CWsp
import Data.BAByNF.ABNF.Rules.DecVal.Test qualified as ABNF.Rules.DecVal
import Data.BAByNF.ABNF.Rules.DefinedAs.Test qualified as ABNF.Rules.DefinedAs
import Data.BAByNF.ABNF.Rules.Element.Test qualified as ABNF.Rules.Element
import Data.BAByNF.ABNF.Rules.Group.Test qualified as ABNF.Rules.Group
import Data.BAByNF.ABNF.Rules.HexVal.Test qualified as ABNF.Rules.HexVal
import Data.BAByNF.ABNF.Rules.NumVal.Test qualified as ABNF.Rules.NumVal
import Data.BAByNF.ABNF.Rules.Option.Test qualified as ABNF.Rules.Option
import Data.BAByNF.ABNF.Rules.ProseVal.Test qualified as ABNF.Rules.ProseVal
import Data.BAByNF.ABNF.Rules.QuotedString.Test qualified as ABNF.Rules.QuotedString
import Data.BAByNF.ABNF.Rules.Repeat.Test qualified as ABNF.Rules.Repeat
import Data.BAByNF.ABNF.Rules.Repetition.Test qualified as ABNF.Rules.Repetition
import Data.BAByNF.ABNF.Rules.Rule.Test qualified as ABNF.Rules.Rule
import Data.BAByNF.ABNF.Rules.Rulelist.Test qualified as ABNF.Rules.Rulelist
import Data.BAByNF.ABNF.Rules.Rulename.Test qualified as ABNF.Rules.Rulename
import Data.BAByNF.Util.Ascii.Test qualified as Util.Ascii

main :: IO ()
main = Tasty.defaultMain $
    Tasty.testGroup "babynf-test" 
        [ Tasty.testGroup "module-tests"
            [ ABNF.Rules.Alternation.testModule
            , ABNF.Rules.BinVal.testModule
            , ABNF.Rules.CaseInsensitiveString.testModule
            , ABNF.Rules.CaseSensitiveString.testModule
            , ABNF.Rules.CharVal.testModule
            , ABNF.Rules.CNl.testModule
            , ABNF.Rules.Comment.testModule
            , ABNF.Rules.CWsp.testModule
            , ABNF.Rules.DecVal.testModule
            , ABNF.Rules.DefinedAs.testModule
            , ABNF.Rules.Element.testModule
            , ABNF.Rules.Group.testModule
            , ABNF.Rules.HexVal.testModule
            , ABNF.Rules.NumVal.testModule
            , ABNF.Rules.Option.testModule
            , ABNF.Rules.ProseVal.testModule
            , ABNF.Rules.QuotedString.testModule
            , ABNF.Rules.Repeat.testModule
            , ABNF.Rules.Repetition.testModule
            , ABNF.Rules.Rule.testModule
            , ABNF.Rules.Rulelist.testModule
            , ABNF.Rules.Rulename.testModule
            , Util.Ascii.testModule
            ]
        ]