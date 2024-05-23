module Data.BAByNF.ABNF.Core where

import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Util.Hex qualified as Hex

import Data.BAByNF.ABNF qualified as ABNF

import qualified Data.BAByNF.ABNF.Grammar as Grammar

rules :: [ABNF.Rule]
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

ruleRefs :: [ABNF.Rulename]
ruleRefs = map (\(ABNF.Rule r _ _) -> r) rules

alphaRef :: ABNF.Rulename
alphaRef = ABNF.Rulename (Ascii.stringAsBytesUnsafe  "ALPHA")

alphaRule :: ABNF.Rule
alphaRule = ABNF.Rule alphaRef ABNF.BasicDefinition .
    ABNF.Elements . ABNF.Alternation $ 
        [ ABNF.Concatenation 
            . List.singleton 
            . ABNF.Repetition ABNF.NoRepeat
            . ABNF.NumValElement 
            . ABNF.HexNumVal 
            $ ABNF.RangeHexVal (Hex.Seq [Hex.X4, Hex.X1]) (Hex.Seq [Hex.X5, Hex.XA])
        , ABNF.Concatenation 
            . List.singleton 
            . ABNF.Repetition ABNF.NoRepeat
            . ABNF.NumValElement 
            . ABNF.HexNumVal 
            $ ABNF.RangeHexVal (Hex.Seq [Hex.X6, Hex.X1]) (Hex.Seq [Hex.X7, Hex.XA])
        ]

bitRef :: ABNF.Rulename
bitRef = ABNF.Rulename (Ascii.stringAsBytesUnsafe  "BIT")

bitRule :: ABNF.Rule
bitRule = ABNF.Rule bitRef ABNF.BasicDefinition
    $ ABNF.Elements
    $ ABNF.Alternation
        [ ABNF.Concatenation 
                . List.singleton 
                . ABNF.Repetition ABNF.NoRepeat
                . ABNF.CharValElement
                . ABNF.CaseInsensitiveCharVal
                . ABNF.CaseInsensitiveString
                . ABNF.QuotedString
                $ Ascii.bs '0'
        , ABNF.Concatenation 
                . List.singleton 
                . ABNF.Repetition ABNF.NoRepeat
                . ABNF.CharValElement
                . ABNF.CaseInsensitiveCharVal
                . ABNF.CaseInsensitiveString
                . ABNF.QuotedString
                $ Ascii.bs '1'
        ]

charRef :: ABNF.Rulename
charRef = ABNF.Rulename (Ascii.stringAsBytesUnsafe  "CHAR")

charRule :: ABNF.Rule
charRule = ABNF.Rule charRef ABNF.BasicDefinition
    $ ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation
    . List.singleton
    . ABNF.Repetition ABNF.NoRepeat
    . ABNF.NumValElement
    . ABNF.HexNumVal
    $ ABNF.RangeHexVal (Hex.Seq [Hex.X0, Hex.X1]) (Hex.Seq [Hex.X7, Hex.XF])

crRef :: ABNF.Rulename
crRef = ABNF.Rulename (Ascii.stringAsBytesUnsafe  "CR")

crRule :: ABNF.Rule
crRule = ABNF.Rule crRef ABNF.BasicDefinition
    $ ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation
    . List.singleton
    . ABNF.Repetition ABNF.NoRepeat
    . ABNF.NumValElement
    . ABNF.HexNumVal
    $ ABNF.SeqHexVal [Hex.Seq [Hex.X0, Hex.XD]]

crlfRef :: ABNF.Rulename
crlfRef = ABNF.Rulename (Ascii.stringAsBytesUnsafe  "CRLF")

crlfRule :: ABNF.Rule
crlfRule = ABNF.Rule crlfRef ABNF.BasicDefinition
    $ ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    $ ABNF.Concatenation 
        [ ABNF.Repetition ABNF.NoRepeat
            $ ABNF.RulenameElement crRef
        , ABNF.Repetition ABNF.NoRepeat
            $ ABNF.RulenameElement lfRef
        ]

ctlRef :: ABNF.Rulename
ctlRef = ABNF.Rulename (Ascii.stringAsBytesUnsafe  "CTL")

ctlRule :: ABNF.Rule
ctlRule = ABNF.Rule ctlRef ABNF.BasicDefinition
    $ ABNF.Elements
    $ ABNF.Alternation
        [ ABNF.Concatenation 
            . List.singleton
            . ABNF.Repetition ABNF.NoRepeat
            . ABNF.NumValElement
            . ABNF.HexNumVal
            $ ABNF.RangeHexVal (Hex.Seq [Hex.X0, Hex.X0]) (Hex.Seq [Hex.X1, Hex.XF])
        , ABNF.Concatenation 
            . List.singleton
            . ABNF.Repetition ABNF.NoRepeat
            . ABNF.NumValElement
            . ABNF.HexNumVal
            $ ABNF.SeqHexVal [Hex.Seq [Hex.X7, Hex.XF]]
        ]

digitRef :: ABNF.Rulename
digitRef = ABNF.Rulename (Ascii.stringAsBytesUnsafe  "DIGIT")

digitRule :: ABNF.Rule
digitRule = ABNF.Rule digitRef ABNF.BasicDefinition
    $ ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation
    . List.singleton
    . ABNF.Repetition ABNF.NoRepeat
    . ABNF.NumValElement
    . ABNF.HexNumVal
    $ ABNF.RangeHexVal (Hex.Seq [Hex.X3, Hex.X0]) (Hex.Seq [Hex.X3, Hex.X9])

dquoteRef :: ABNF.Rulename
dquoteRef = ABNF.Rulename (Ascii.stringAsBytesUnsafe  "DQUOTE")

dquoteRule :: ABNF.Rule
dquoteRule = ABNF.Rule dquoteRef ABNF.BasicDefinition
    $ ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation
    . List.singleton
    . ABNF.Repetition ABNF.NoRepeat
    . ABNF.NumValElement
    . ABNF.HexNumVal
    $ ABNF.SeqHexVal [Hex.Seq [Hex.X2, Hex.X2]]

hexdigRef :: ABNF.Rulename
hexdigRef = ABNF.Rulename (Ascii.stringAsBytesUnsafe  "HEXDIG")

hexdigRule :: ABNF.Rule
hexdigRule = ABNF.Rule hexdigRef ABNF.BasicDefinition
    $ ABNF.Elements
    $ ABNF.Alternation
        [ ABNF.Concatenation 
                . List.singleton 
                . ABNF.Repetition ABNF.NoRepeat
                $ ABNF.RulenameElement digitRef
        , ABNF.Concatenation 
                . List.singleton 
                . ABNF.Repetition ABNF.NoRepeat
                . ABNF.CharValElement
                . ABNF.CaseInsensitiveCharVal
                . ABNF.CaseInsensitiveString
                . ABNF.QuotedString
                $ Ascii.bs 'A'
        , ABNF.Concatenation 
                . List.singleton 
                . ABNF.Repetition ABNF.NoRepeat
                . ABNF.CharValElement
                . ABNF.CaseInsensitiveCharVal
                . ABNF.CaseInsensitiveString
                . ABNF.QuotedString
                $ Ascii.bs 'B'
        , ABNF.Concatenation 
                . List.singleton 
                . ABNF.Repetition ABNF.NoRepeat
                . ABNF.CharValElement
                . ABNF.CaseInsensitiveCharVal
                . ABNF.CaseInsensitiveString
                . ABNF.QuotedString
                $ Ascii.bs 'C'
        , ABNF.Concatenation 
                . List.singleton 
                . ABNF.Repetition ABNF.NoRepeat
                . ABNF.CharValElement
                . ABNF.CaseInsensitiveCharVal
                . ABNF.CaseInsensitiveString
                . ABNF.QuotedString
                $ Ascii.bs 'D'
        , ABNF.Concatenation 
                . List.singleton 
                . ABNF.Repetition ABNF.NoRepeat
                . ABNF.CharValElement
                . ABNF.CaseInsensitiveCharVal
                . ABNF.CaseInsensitiveString
                . ABNF.QuotedString
                $ Ascii.bs 'E'
        , ABNF.Concatenation 
                . List.singleton 
                . ABNF.Repetition ABNF.NoRepeat
                . ABNF.CharValElement
                . ABNF.CaseInsensitiveCharVal
                . ABNF.CaseInsensitiveString
                . ABNF.QuotedString
                $ Ascii.bs 'F'
        ]

htabRef :: ABNF.Rulename
htabRef = ABNF.Rulename (Ascii.stringAsBytesUnsafe  "HTAB")

htabRule :: ABNF.Rule
htabRule = ABNF.Rule htabRef ABNF.BasicDefinition
    $ ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation
    . List.singleton
    . ABNF.Repetition ABNF.NoRepeat
    . ABNF.NumValElement
    . ABNF.HexNumVal
    $ ABNF.SeqHexVal [Hex.Seq [Hex.X0, Hex.X9]]

lfRef :: ABNF.Rulename
lfRef = ABNF.Rulename (Ascii.stringAsBytesUnsafe  "LF")

lfRule :: ABNF.Rule
lfRule = ABNF.Rule lfRef ABNF.BasicDefinition
    $ ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation
    . List.singleton
    . ABNF.Repetition ABNF.NoRepeat
    . ABNF.NumValElement
    . ABNF.HexNumVal
    $ ABNF.SeqHexVal [Hex.Seq [Hex.X0, Hex.XA]]

lwspRef :: ABNF.Rulename
lwspRef = ABNF.Rulename (Ascii.stringAsBytesUnsafe  "LWSP")

lwspRule :: ABNF.Rule
lwspRule = ABNF.Rule lwspRef ABNF.BasicDefinition
    $ ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation
    . List.singleton
    . ABNF.Repetition (ABNF.RangedRepeat ABNF.UnBound ABNF.UnBound)
    . ABNF.GroupElement
    . ABNF.Group
    $ ABNF.Alternation
        [ ABNF.Concatenation 
                . List.singleton 
                . ABNF.Repetition ABNF.NoRepeat
                $ ABNF.RulenameElement wspRef
        , ABNF.Concatenation
            [ ABNF.Repetition ABNF.NoRepeat
                $ ABNF.RulenameElement crlfRef
            , ABNF.Repetition ABNF.NoRepeat
                $ ABNF.RulenameElement wspRef
            ]
        ]
    

octetRef :: ABNF.Rulename
octetRef = ABNF.Rulename (Ascii.stringAsBytesUnsafe  "OCTET")

octetRule :: ABNF.Rule
octetRule = ABNF.Rule octetRef ABNF.BasicDefinition
    $ ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation
    . List.singleton
    . ABNF.Repetition ABNF.NoRepeat
    . ABNF.NumValElement
    . ABNF.HexNumVal
    $ ABNF.RangeHexVal (Hex.Seq [Hex.X0, Hex.X0]) (Hex.Seq [Hex.XF, Hex.XF])

spRef :: ABNF.Rulename
spRef = ABNF.Rulename (Ascii.stringAsBytesUnsafe  "SP")

spRule :: ABNF.Rule
spRule = ABNF.Rule spRef ABNF.BasicDefinition
    $ ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation
    . List.singleton
    . ABNF.Repetition ABNF.NoRepeat
    . ABNF.NumValElement
    . ABNF.HexNumVal
    $ ABNF.SeqHexVal [Hex.Seq [Hex.X2, Hex.X0]]

vcharRef :: ABNF.Rulename
vcharRef = ABNF.Rulename (Ascii.stringAsBytesUnsafe  "VCHAR")

vcharRule :: ABNF.Rule
vcharRule = ABNF.Rule vcharRef ABNF.BasicDefinition
    $ ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation
    . List.singleton
    . ABNF.Repetition ABNF.NoRepeat
    . ABNF.NumValElement
    . ABNF.HexNumVal
    $ ABNF.RangeHexVal (Hex.Seq [Hex.X2, Hex.X1]) (Hex.Seq [Hex.X7, Hex.XE])


wspRef :: ABNF.Rulename
wspRef = ABNF.Rulename (Ascii.stringAsBytesUnsafe  "WSP")

wspRule :: ABNF.Rule
wspRule = ABNF.Rule wspRef ABNF.BasicDefinition
        $ ABNF.Elements
    $ ABNF.Alternation
        [ ABNF.Concatenation 
                . List.singleton 
                . ABNF.Repetition ABNF.NoRepeat
                $ ABNF.RulenameElement spRef
        , ABNF.Concatenation 
                . List.singleton 
                . ABNF.Repetition ABNF.NoRepeat
                $ ABNF.RulenameElement htabRef
        ]