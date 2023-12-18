module Data.BAByNF.ABNF.Core where

import qualified Data.BAByNF.ABNF.Grammar as Grammar

rules :: [Grammar.RuleDecl]
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

ruleRefs :: [Grammar.RuleRef]
ruleRefs = map (\(Grammar.RuleDecl r _) -> r) rules

alphaRef :: Grammar.RuleRef
alphaRef = Grammar.ref "ALPHA"

alphaRule :: Grammar.RuleDecl
alphaRule = Grammar.RuleDecl alphaRef (Grammar.RuleDef $ (Grammar.rng 65 90) Grammar.<||> (Grammar.rng 97 122))

bitRef :: Grammar.RuleRef
bitRef = Grammar.ref "BIT"

bitRule :: Grammar.RuleDecl
bitRule = Grammar.RuleDecl bitRef (Grammar.RuleDef $ (Grammar.str "0") Grammar.<||> (Grammar.str "1"))

charRef :: Grammar.RuleRef
charRef = Grammar.ref "CHAR"

charRule :: Grammar.RuleDecl
charRule = Grammar.RuleDecl charRef (Grammar.RuleDef $ Grammar.rng 1 127)

crRef :: Grammar.RuleRef
crRef = Grammar.ref "CR"

crRule :: Grammar.RuleDecl
crRule = Grammar.RuleDecl crRef (Grammar.RuleDef $ Grammar.byte 13)

crlfRef :: Grammar.RuleRef
crlfRef = Grammar.ref "CRLF"

crlfRule :: Grammar.RuleDecl
crlfRule = Grammar.RuleDecl crlfRef (Grammar.RuleDef $ Grammar.asAlt $ crRef Grammar.<..> lfRef)

ctlRef :: Grammar.RuleRef
ctlRef = Grammar.ref "CTL"

ctlRule :: Grammar.RuleDecl
ctlRule = Grammar.RuleDecl ctlRef (Grammar.RuleDef $ (Grammar.rng 0 31) Grammar.<||> (Grammar.byte 127))

digitRef :: Grammar.RuleRef
digitRef = Grammar.ref "DIGIT"

digitRule :: Grammar.RuleDecl
digitRule = Grammar.RuleDecl digitRef (Grammar.RuleDef $ Grammar.rng 48 57)

dquoteRef :: Grammar.RuleRef
dquoteRef = Grammar.ref "DQUOTE"

dquoteRule :: Grammar.RuleDecl
dquoteRule = Grammar.RuleDecl dquoteRef (Grammar.RuleDef $ Grammar.byte 34)

hexdigRef :: Grammar.RuleRef
hexdigRef = Grammar.ref "HEXDIG"

hexdigRule :: Grammar.RuleDecl
hexdigRule = Grammar.RuleDecl hexdigRef (Grammar.RuleDef $ Grammar.asAlt digitRef Grammar.<||> Grammar.str "A" Grammar.<||> Grammar.str "B" Grammar.<||> Grammar.str "C" Grammar.<||> Grammar.str "D" Grammar.<||> Grammar.str "E" Grammar.<||> Grammar.str "F")

htabRef :: Grammar.RuleRef
htabRef = Grammar.ref "HTAB"

htabRule :: Grammar.RuleDecl
htabRule = Grammar.RuleDecl htabRef (Grammar.RuleDef $ Grammar.byte 9)

lfRef :: Grammar.RuleRef
lfRef = Grammar.ref "LF"

lfRule :: Grammar.RuleDecl
lfRule = Grammar.RuleDecl lfRef (Grammar.RuleDef $ Grammar.byte 10)

lwspRef :: Grammar.RuleRef
lwspRef = Grammar.ref "LWSP"

lwspRule :: Grammar.RuleDecl
lwspRule = Grammar.RuleDecl lwspRef (Grammar.RuleDef . Grammar.zeroOrMore . Grammar.group $ wspRef Grammar.<|||> (crlfRef Grammar.<..> wspRef))

octetRef :: Grammar.RuleRef
octetRef = Grammar.ref "OCTET"

octetRule :: Grammar.RuleDecl
octetRule = Grammar.RuleDecl octetRef (Grammar.RuleDef $ Grammar.rng 0 255)

spRef :: Grammar.RuleRef
spRef = Grammar.ref "SP"

spRule :: Grammar.RuleDecl
spRule = Grammar.RuleDecl spRef (Grammar.RuleDef $ Grammar.byte 32)

vcharRef :: Grammar.RuleRef
vcharRef = Grammar.ref "VCHAR"

vcharRule :: Grammar.RuleDecl
vcharRule = Grammar.RuleDecl vcharRef (Grammar.RuleDef $ Grammar.rng 33 126)


wspRef :: Grammar.RuleRef
wspRef = Grammar.ref "WSP"

wspRule :: Grammar.RuleDecl
wspRule = Grammar.RuleDecl wspRef (Grammar.RuleDef $ spRef Grammar.<|||> htabRef)
