module Data.BAByNF.Grammar.Core where

import qualified Data.BAByNF.Grammar as G

rules :: [G.RuleDecl]
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


alphaRef :: G.RuleRef
alphaRef = G.ref "ALPHA"

alphaRule :: G.RuleDecl
alphaRule = G.RuleDecl alphaRef (G.RuleDef $ (G.rng 65 90) G.<||> (G.rng 97 122))

bitRef :: G.RuleRef
bitRef = G.ref "BIT"

bitRule :: G.RuleDecl
bitRule = G.RuleDecl bitRef (G.RuleDef $ (G.str "0") G.<||> (G.str "1"))

charRef :: G.RuleRef
charRef = G.ref "CHAR"

charRule :: G.RuleDecl
charRule = G.RuleDecl charRef (G.RuleDef $ G.rng 1 127)

crRef :: G.RuleRef
crRef = G.ref "CR"

crRule :: G.RuleDecl
crRule = G.RuleDecl crRef (G.RuleDef $ G.byte 13)

crlfRef :: G.RuleRef
crlfRef = G.ref "CRLF"

crlfRule :: G.RuleDecl
crlfRule = G.RuleDecl crlfRef (G.RuleDef $ G.asAlt $ crRef G.<..> lfRef)

ctlRef :: G.RuleRef
ctlRef = G.ref "CTL"

ctlRule :: G.RuleDecl
ctlRule = G.RuleDecl ctlRef (G.RuleDef $ (G.rng 0 31) G.<||> (G.byte 127))

digitRef :: G.RuleRef
digitRef = G.ref "DIGIT"

digitRule :: G.RuleDecl
digitRule = G.RuleDecl digitRef (G.RuleDef $ G.rng 48 57)

dquoteRef :: G.RuleRef
dquoteRef = G.ref "DQUOTE"

dquoteRule :: G.RuleDecl
dquoteRule = G.RuleDecl dquoteRef (G.RuleDef $ G.byte 34)

hexdigRef :: G.RuleRef
hexdigRef = G.ref "HEXDIG"

hexdigRule :: G.RuleDecl
hexdigRule = G.RuleDecl hexdigRef (G.RuleDef $ G.asAlt digitRef G.<||> G.str "A" G.<||> G.str "B" G.<||> G.str "C" G.<||> G.str "D" G.<||> G.str "E" G.<||> G.str "F")

htabRef :: G.RuleRef
htabRef = G.ref "HTAB"

htabRule :: G.RuleDecl
htabRule = G.RuleDecl htabRef (G.RuleDef $ G.byte 9)

lfRef :: G.RuleRef
lfRef = G.ref "LF"

lfRule :: G.RuleDecl
lfRule = G.RuleDecl lfRef (G.RuleDef $ G.byte 10)

lwspRef :: G.RuleRef
lwspRef = G.ref "LWSP"

lwspRule :: G.RuleDecl
lwspRule = G.RuleDecl lwspRef (G.RuleDef . G.zeroOrMore . G.group $ wspRef G.<|||> (crlfRef G.<..> wspRef))

octetRef :: G.RuleRef
octetRef = G.ref "OCTET"

octetRule :: G.RuleDecl
octetRule = G.RuleDecl octetRef (G.RuleDef $ G.rng 0 255)

spRef :: G.RuleRef
spRef = G.ref "SP"

spRule :: G.RuleDecl
spRule = G.RuleDecl spRef (G.RuleDef $ G.byte 32)

vcharRef :: G.RuleRef
vcharRef = G.ref "VCHAR"

vcharRule :: G.RuleDecl
vcharRule = G.RuleDecl vcharRef (G.RuleDef $ G.rng 33 126)


wspRef :: G.RuleRef
wspRef = G.ref "WSP"

wspRule :: G.RuleDecl
wspRule = G.RuleDecl wspRef (G.RuleDef $ spRef G.<|||> htabRef)
