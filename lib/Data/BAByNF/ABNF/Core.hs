module Data.BAByNF.ABNF.Core
    ( rules
    , ruleRefs
    , alphaRef
    , alphaRule
    , bitRef
    , bitRule
    , charRef
    , charRule
    , crRef
    , crRule
    , crlfRef
    , crlfRule
    , ctlRef
    , ctlRule
    , digitRef
    , digitRule
    , dquoteRef
    , dquoteRule
    , hexdigRef
    , hexdigRule
    , htabRef
    , htabRule
    , lfRef
    , lfRule
    , lwspRef
    , lwspRule
    , octetRef
    , octetRule
    , spRef
    , spRule
    , vcharRef
    , vcharRule
    , wspRef
    , wspRule
    ) where

import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Util.Hex qualified as Hex
import Data.BAByNF.ABNF.Model qualified as Model

rules :: [Model.Rule]
rules =
    [ alphaRule
    , bitRule
    , charRule
    , crRule
    , crlfRule
    , ctlRule
    , digitRule
    , dquoteRule
    , hexdigRule
    , htabRule
    , lfRule
    , lwspRule
    , octetRule
    , spRule
    , vcharRule
    , wspRule
    ]

ruleRefs :: [Model.Rulename]
ruleRefs = map (\(Model.Rule r _ _) -> r) rules

alphaRef :: Model.Rulename
alphaRef = Model.Rulename (Ascii.stringAsBytesUnsafe  "ALPHA")

alphaRule :: Model.Rule
alphaRule = Model.Rule alphaRef Model.BasicDefinition .
    Model.Elements . Model.Alternation $ 
        [ Model.Concatenation 
            . List.singleton 
            . Model.Repetition Model.NoRepeat
            . Model.NumValElement 
            . Model.HexNumVal 
            $ Model.RangeHexVal (Hex.Seq [Hex.X4, Hex.X1]) (Hex.Seq [Hex.X5, Hex.XA])
        , Model.Concatenation 
            . List.singleton 
            . Model.Repetition Model.NoRepeat
            . Model.NumValElement 
            . Model.HexNumVal 
            $ Model.RangeHexVal (Hex.Seq [Hex.X6, Hex.X1]) (Hex.Seq [Hex.X7, Hex.XA])
        ]

bitRef :: Model.Rulename
bitRef = Model.Rulename (Ascii.stringAsBytesUnsafe  "BIT")

bitRule :: Model.Rule
bitRule = Model.Rule bitRef Model.BasicDefinition
    $ Model.Elements
    $ Model.Alternation
        [ Model.Concatenation 
                . List.singleton 
                . Model.Repetition Model.NoRepeat
                . Model.CharValElement
                . Model.CaseInsensitiveCharVal
                . Model.CaseInsensitiveString
                . Model.QuotedString
                $ Ascii.bs '0'
        , Model.Concatenation 
                . List.singleton 
                . Model.Repetition Model.NoRepeat
                . Model.CharValElement
                . Model.CaseInsensitiveCharVal
                . Model.CaseInsensitiveString
                . Model.QuotedString
                $ Ascii.bs '1'
        ]

charRef :: Model.Rulename
charRef = Model.Rulename (Ascii.stringAsBytesUnsafe  "CHAR")

charRule :: Model.Rule
charRule = Model.Rule charRef Model.BasicDefinition
    $ Model.Elements
    . Model.Alternation
    . List.singleton
    . Model.Concatenation
    . List.singleton
    . Model.Repetition Model.NoRepeat
    . Model.NumValElement
    . Model.HexNumVal
    $ Model.RangeHexVal (Hex.Seq [Hex.X0, Hex.X1]) (Hex.Seq [Hex.X7, Hex.XF])

crRef :: Model.Rulename
crRef = Model.Rulename (Ascii.stringAsBytesUnsafe  "CR")

crRule :: Model.Rule
crRule = Model.Rule crRef Model.BasicDefinition
    $ Model.Elements
    . Model.Alternation
    . List.singleton
    . Model.Concatenation
    . List.singleton
    . Model.Repetition Model.NoRepeat
    . Model.NumValElement
    . Model.HexNumVal
    $ Model.SeqHexVal [Hex.Seq [Hex.X0, Hex.XD]]

crlfRef :: Model.Rulename
crlfRef = Model.Rulename (Ascii.stringAsBytesUnsafe  "CRLF")

crlfRule :: Model.Rule
crlfRule = Model.Rule crlfRef Model.BasicDefinition
    $ Model.Elements
    . Model.Alternation
    . List.singleton
    $ Model.Concatenation 
        [ Model.Repetition Model.NoRepeat
            $ Model.RulenameElement crRef
        , Model.Repetition Model.NoRepeat
            $ Model.RulenameElement lfRef
        ]

ctlRef :: Model.Rulename
ctlRef = Model.Rulename (Ascii.stringAsBytesUnsafe  "CTL")

ctlRule :: Model.Rule
ctlRule = Model.Rule ctlRef Model.BasicDefinition
    $ Model.Elements
    $ Model.Alternation
        [ Model.Concatenation 
            . List.singleton
            . Model.Repetition Model.NoRepeat
            . Model.NumValElement
            . Model.HexNumVal
            $ Model.RangeHexVal (Hex.Seq [Hex.X0, Hex.X0]) (Hex.Seq [Hex.X1, Hex.XF])
        , Model.Concatenation 
            . List.singleton
            . Model.Repetition Model.NoRepeat
            . Model.NumValElement
            . Model.HexNumVal
            $ Model.SeqHexVal [Hex.Seq [Hex.X7, Hex.XF]]
        ]

digitRef :: Model.Rulename
digitRef = Model.Rulename (Ascii.stringAsBytesUnsafe  "DIGIT")

digitRule :: Model.Rule
digitRule = Model.Rule digitRef Model.BasicDefinition
    $ Model.Elements
    . Model.Alternation
    . List.singleton
    . Model.Concatenation
    . List.singleton
    . Model.Repetition Model.NoRepeat
    . Model.NumValElement
    . Model.HexNumVal
    $ Model.RangeHexVal (Hex.Seq [Hex.X3, Hex.X0]) (Hex.Seq [Hex.X3, Hex.X9])

dquoteRef :: Model.Rulename
dquoteRef = Model.Rulename (Ascii.stringAsBytesUnsafe  "DQUOTE")

dquoteRule :: Model.Rule
dquoteRule = Model.Rule dquoteRef Model.BasicDefinition
    $ Model.Elements
    . Model.Alternation
    . List.singleton
    . Model.Concatenation
    . List.singleton
    . Model.Repetition Model.NoRepeat
    . Model.NumValElement
    . Model.HexNumVal
    $ Model.SeqHexVal [Hex.Seq [Hex.X2, Hex.X2]]

hexdigRef :: Model.Rulename
hexdigRef = Model.Rulename (Ascii.stringAsBytesUnsafe  "HEXDIG")

hexdigRule :: Model.Rule
hexdigRule = Model.Rule hexdigRef Model.BasicDefinition
    $ Model.Elements
    $ Model.Alternation
        [ Model.Concatenation 
                . List.singleton 
                . Model.Repetition Model.NoRepeat
                $ Model.RulenameElement digitRef
        , Model.Concatenation 
                . List.singleton 
                . Model.Repetition Model.NoRepeat
                . Model.CharValElement
                . Model.CaseInsensitiveCharVal
                . Model.CaseInsensitiveString
                . Model.QuotedString
                $ Ascii.bs 'A'
        , Model.Concatenation 
                . List.singleton 
                . Model.Repetition Model.NoRepeat
                . Model.CharValElement
                . Model.CaseInsensitiveCharVal
                . Model.CaseInsensitiveString
                . Model.QuotedString
                $ Ascii.bs 'B'
        , Model.Concatenation 
                . List.singleton 
                . Model.Repetition Model.NoRepeat
                . Model.CharValElement
                . Model.CaseInsensitiveCharVal
                . Model.CaseInsensitiveString
                . Model.QuotedString
                $ Ascii.bs 'C'
        , Model.Concatenation 
                . List.singleton 
                . Model.Repetition Model.NoRepeat
                . Model.CharValElement
                . Model.CaseInsensitiveCharVal
                . Model.CaseInsensitiveString
                . Model.QuotedString
                $ Ascii.bs 'D'
        , Model.Concatenation 
                . List.singleton 
                . Model.Repetition Model.NoRepeat
                . Model.CharValElement
                . Model.CaseInsensitiveCharVal
                . Model.CaseInsensitiveString
                . Model.QuotedString
                $ Ascii.bs 'E'
        , Model.Concatenation 
                . List.singleton 
                . Model.Repetition Model.NoRepeat
                . Model.CharValElement
                . Model.CaseInsensitiveCharVal
                . Model.CaseInsensitiveString
                . Model.QuotedString
                $ Ascii.bs 'F'
        ]

htabRef :: Model.Rulename
htabRef = Model.Rulename (Ascii.stringAsBytesUnsafe  "HTAB")

htabRule :: Model.Rule
htabRule = Model.Rule htabRef Model.BasicDefinition
    $ Model.Elements
    . Model.Alternation
    . List.singleton
    . Model.Concatenation
    . List.singleton
    . Model.Repetition Model.NoRepeat
    . Model.NumValElement
    . Model.HexNumVal
    $ Model.SeqHexVal [Hex.Seq [Hex.X0, Hex.X9]]

lfRef :: Model.Rulename
lfRef = Model.Rulename (Ascii.stringAsBytesUnsafe  "LF")

lfRule :: Model.Rule
lfRule = Model.Rule lfRef Model.BasicDefinition
    $ Model.Elements
    . Model.Alternation
    . List.singleton
    . Model.Concatenation
    . List.singleton
    . Model.Repetition Model.NoRepeat
    . Model.NumValElement
    . Model.HexNumVal
    $ Model.SeqHexVal [Hex.Seq [Hex.X0, Hex.XA]]

lwspRef :: Model.Rulename
lwspRef = Model.Rulename (Ascii.stringAsBytesUnsafe  "LWSP")

lwspRule :: Model.Rule
lwspRule = Model.Rule lwspRef Model.BasicDefinition
    $ Model.Elements
    . Model.Alternation
    . List.singleton
    . Model.Concatenation
    . List.singleton
    . Model.Repetition (Model.RangedRepeat Model.UnBound Model.UnBound)
    . Model.GroupElement
    . Model.Group
    $ Model.Alternation
        [ Model.Concatenation 
                . List.singleton 
                . Model.Repetition Model.NoRepeat
                $ Model.RulenameElement wspRef
        , Model.Concatenation
            [ Model.Repetition Model.NoRepeat
                $ Model.RulenameElement crlfRef
            , Model.Repetition Model.NoRepeat
                $ Model.RulenameElement wspRef
            ]
        ]
    

octetRef :: Model.Rulename
octetRef = Model.Rulename (Ascii.stringAsBytesUnsafe  "OCTET")

octetRule :: Model.Rule
octetRule = Model.Rule octetRef Model.BasicDefinition
    $ Model.Elements
    . Model.Alternation
    . List.singleton
    . Model.Concatenation
    . List.singleton
    . Model.Repetition Model.NoRepeat
    . Model.NumValElement
    . Model.HexNumVal
    $ Model.RangeHexVal (Hex.Seq [Hex.X0, Hex.X0]) (Hex.Seq [Hex.XF, Hex.XF])

spRef :: Model.Rulename
spRef = Model.Rulename (Ascii.stringAsBytesUnsafe  "SP")

spRule :: Model.Rule
spRule = Model.Rule spRef Model.BasicDefinition
    $ Model.Elements
    . Model.Alternation
    . List.singleton
    . Model.Concatenation
    . List.singleton
    . Model.Repetition Model.NoRepeat
    . Model.NumValElement
    . Model.HexNumVal
    $ Model.SeqHexVal [Hex.Seq [Hex.X2, Hex.X0]]

vcharRef :: Model.Rulename
vcharRef = Model.Rulename (Ascii.stringAsBytesUnsafe  "VCHAR")

vcharRule :: Model.Rule
vcharRule = Model.Rule vcharRef Model.BasicDefinition
    $ Model.Elements
    . Model.Alternation
    . List.singleton
    . Model.Concatenation
    . List.singleton
    . Model.Repetition Model.NoRepeat
    . Model.NumValElement
    . Model.HexNumVal
    $ Model.RangeHexVal (Hex.Seq [Hex.X2, Hex.X1]) (Hex.Seq [Hex.X7, Hex.XE])


wspRef :: Model.Rulename
wspRef = Model.Rulename (Ascii.stringAsBytesUnsafe  "WSP")

wspRule :: Model.Rule
wspRule = Model.Rule wspRef Model.BasicDefinition
        $ Model.Elements
    $ Model.Alternation
        [ Model.Concatenation 
                . List.singleton 
                . Model.Repetition Model.NoRepeat
                $ Model.RulenameElement spRef
        , Model.Concatenation 
                . List.singleton 
                . Model.Repetition Model.NoRepeat
                $ Model.RulenameElement htabRef
        ]