module Data.BAByNF.ABNF.Rules.Element
    ( ref
    , rule
    , fromTree
    ) where

import Data.Functor ((<&>))
-- import Data.BAByNF.Core.Tree (Tree)
-- import Data.BAByNF.Core.Tree qualified as Tree
-- import Data.BAByNF.ABNF.Grammar ((|?))
-- import Data.BAByNF.ABNF.Grammar qualified as Grammar
import Data.BAByNF.Util.Stream qualified as Stream
import {-# SOURCE #-} Data.BAByNF.ABNF.Rules.Option qualified as Option
import Data.BAByNF.ABNF.Rules.CharVal qualified as CharVal
import Data.BAByNF.ABNF.Rules.NumVal qualified as NumVal
import Data.BAByNF.ABNF.Rules.Rulename qualified as Rulename
import Data.BAByNF.ABNF.Rules.Group qualified as Group
import Data.BAByNF.ABNF.Rules.ProseVal qualified as ProseVal
import Data.List qualified as List
import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Util.List qualified as Util.List
import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF.Rules.CWsp qualified as CWsp
import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF (Element(RulenameElement))
import Data.BAByNF.Core.Ref qualified as Ref


ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe  "element")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition 
    . ABNF.Elements 
    . ABNF.Alternation
    $ 
        [ ABNF.Concatenation 
            . List.singleton
            . ABNF.Repetition ABNF.NoRepeat
            $ RulenameElement Rulename.ref
        , ABNF.Concatenation 
            . List.singleton
            . ABNF.Repetition ABNF.NoRepeat
            $ RulenameElement Group.ref
        , ABNF.Concatenation 
            . List.singleton
            . ABNF.Repetition ABNF.NoRepeat
            $ RulenameElement Option.ref
        , ABNF.Concatenation 
            . List.singleton
            . ABNF.Repetition ABNF.NoRepeat
            $ RulenameElement CharVal.ref
        , ABNF.Concatenation 
            . List.singleton
            . ABNF.Repetition ABNF.NoRepeat
            $ RulenameElement NumVal.ref
        , ABNF.Concatenation 
            . List.singleton
            . ABNF.Repetition ABNF.NoRepeat
            $ RulenameElement ProseVal.ref
        ]

fromTree :: Tree ABNF.Rulename -> Either String ABNF.Element
fromTree tree = 
    case Tree.nodes tree of
        [Tree.RefNode r subtree] -> 
            if Ref.eq r Rulename.ref then Right . ABNF.RulenameElement . Rulename.fromTree $ subtree  
            else if Ref.eq r Group.ref then Group.fromTree subtree <&> ABNF.GroupElement
            else if Ref.eq r Option.ref then Option.fromTree subtree <&> ABNF.OptionElement
            else if Ref.eq r CharVal.ref then CharVal.fromTree subtree <&> ABNF.CharValElement
            else if Ref.eq r NumVal.ref then NumVal.fromTree subtree <&> ABNF.NumValElement
            else if Ref.eq r ProseVal.ref then ProseVal.fromTree subtree <&> ABNF.ProseValElement
            else Left "element must be rulename | group | option | char-val | num-val | prose-val"
        _ -> Left "structural mismatch for <element>"
