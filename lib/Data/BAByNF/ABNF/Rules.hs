module Data.BAByNF.ABNF.Rules
    ( rules
    ) where

import Data.BAByNF.ABNF.Core qualified as Core
import Data.BAByNF.ABNF.Rules.Option qualified as Option
import Data.BAByNF.ABNF.Rules.CharVal qualified as CharVal
import Data.BAByNF.ABNF.Rules.NumVal qualified as NumVal
import Data.BAByNF.ABNF.Rules.BinVal qualified as BinVal
import Data.BAByNF.ABNF.Rules.DecVal qualified as DecVal
import Data.BAByNF.ABNF.Rules.HexVal qualified as HexVal
import Data.BAByNF.ABNF.Rules.ProseVal qualified as ProseVal
import Data.BAByNF.ABNF.Rules.Element qualified as Element
import Data.BAByNF.ABNF.Rules.Elements qualified as Elements
import Data.BAByNF.ABNF.Rules.Rule qualified as Rule
import Data.BAByNF.ABNF.Rules.Group qualified as Group
import Data.BAByNF.ABNF.Rules.Repeat qualified as Repeat
import Data.BAByNF.ABNF.Rules.Repetition qualified as Repetition
import Data.BAByNF.ABNF.Rules.Concatenation qualified as Concatenation
import Data.BAByNF.ABNF.Rules.Alternation qualified as Alternation
import Data.BAByNF.ABNF.Rules.Comment qualified as Comment
import Data.BAByNF.ABNF.Rules.CNl qualified as CNl
import Data.BAByNF.ABNF.Rules.CWsp qualified as CWsp
import Data.BAByNF.ABNF.Rules.DefinedAs qualified as DefinedAs
import Data.BAByNF.ABNF.Rules.Rulename qualified as Rulename
import Data.BAByNF.ABNF.Rules.Rulelist qualified as Rulelist
import Data.BAByNF.ABNF.Rules.CaseInsensitiveString qualified as CaseInsensitiveString
import Data.BAByNF.ABNF.Rules.CaseSensitiveString qualified as CaseSensitiveString
import Data.BAByNF.ABNF.Rules.QuotedString qualified as QuotedString
import Data.BAByNF.ABNF.Model qualified as Model

-- todo: move out core rules to opt-in abnf parser configuration element
rules :: Model.Rulelist
rules = Model.Rulelist $ Core.rules ++
            [ Rulelist.rule
            , Rule.rule
            , Rulename.rule
            , DefinedAs.rule
            , Elements.rule
            , CWsp.rule
            , CNl.rule
            , Comment.rule
            , Alternation.rule
            , Concatenation.rule
            , Repetition.rule
            , Repeat.rule
            , Element.rule
            , Group.rule
            , Option.rule
            , CharVal.rule
            , NumVal.rule
            , BinVal.rule
            , DecVal.rule
            , HexVal.rule
            , ProseVal.rule
            , CaseInsensitiveString.rule
            , CaseSensitiveString.rule
            , QuotedString.rule
            ]