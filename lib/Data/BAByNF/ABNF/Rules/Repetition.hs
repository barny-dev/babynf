module Data.BAByNF.ABNF.Rules.Repetition
    ( ref
    , rule
    , fromTree
    ) where

import Data.BAByNF.Util.Ascii qualified as Ascii

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Grammar qualified as Grammar
import Data.BAByNF.ABNF.Rules.Repeat qualified as Repeat
import Data.BAByNF.ABNF.Rules.Element qualified as Element

ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "repetition")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition _ 
--  ref (Grammar.RuleDef . Grammar.asAlt $ (Grammar.opt (Repeat.ref ??)) +! (Element.ref ??))

fromTree :: Tree ABNF.Rulename -> Either String ABNF.Repetition
fromTree tree =
    -- let rd = maybe (Right Nothing) (fmap Just . Repeat.fromTree)  (Tree.getChildWithRef (Grammar.toRef Repeat.ref) tree)
    --     e = Tree.tryGetChildWithRef (Grammar.toRef Element.ref) tree >>= Element.fromTree
    --  in rd >>= \rd' ->
    --      e >>= \e' -> return $ Grammar.Rep rd' e'