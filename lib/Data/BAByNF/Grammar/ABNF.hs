module Data.BAByNF.Grammar.ABNF where

import Data.BAByNF.Grammar
import qualified Data.BAByNF.Grammar.Core as Core

abnfGrammar :: Grammar

abnfGrammar = Grammar $ Core.rules ++
    [ rulelistRule
    , ruleRule
    , rulenameRule
    , definedAsRule
    , elementsRule
    , cWspRule
    , cNlRule
    , commentRule
    , alternationRule
    , concatenationRule
    , repetitionRule
    , repeatRule
    , elementRule
    , groupRule
    , optionRule
    , charValRule
    , numValRule
    , binValRule
    , decValRule
    , hexValRule
    , proseValRule
    ]

rulelistRef :: RuleRef
rulelistRef = ref "rulelist"

rulelistRule :: RuleDecl
rulelistRule = RuleDecl rulelistRef (RuleDef $ oneOrMore . group $ (asElem ruleRef)  |? ((zeroOrMore . asElem $ cWspRef) +! (cNlRef ??)))

    -- , ("rule", asDef $ asConcat (ref "rulename") <.> (ref' "defined-as") <.> (ref' "elements") <.> (ref' "c-nl"))

ruleRef :: RuleRef
ruleRef = ref "rule"

ruleRule :: RuleDecl
ruleRule = RuleDecl ruleRef (RuleDef . asAlt $ rulenameRef +? definedAsRef +? elementsRef +? cNlRef)

rulenameRef :: RuleRef
rulenameRef = ref "rulename"

    -- , ("rulename", asDef $ (ref' "ALPHA") <.> (zeroOrMore . group $ (asAlt (ref "ALPHA") <||> (ref' "DIGIT") <||> (str "-"))))

rulenameRule :: RuleDecl
rulenameRule = RuleDecl rulenameRef (RuleDef . asAlt $ (Core.alphaRef ??) +! (zeroOrMore . group $ Core.alphaRef |? Core.digitRef |! (str "-")))

definedAsRef :: RuleRef
definedAsRef = ref "defined-as"

    -- , ("defined-as", asDef $ (zeroOrMore $ ref' "c-wsp") <.> (group $ (str "=") <||> (str "=/")) <.> (zeroOrMore $ ref' "c-wsp")) --todo: fail to parse? expect = to always succeed before =/

definedAsRule :: RuleDecl
definedAsRule = RuleDecl definedAsRef (RuleDef . asAlt $ (zeroOrMore . asElem $ cWspRef) +! (group $ (str "=") |! (str "=/")) +! (zeroOrMore . asElem $ cWspRef))

elementsRef :: RuleRef
elementsRef = ref "elements"

    -- , ("elements", asDef $ asConcat (ref "alternation") <.> (zeroOrMore $ ref' "c-wsp"))

elementsRule :: RuleDecl
elementsRule = RuleDecl elementsRef (RuleDef . asAlt $ (alternationRef ??) +! (zeroOrMore . asElem $ cWspRef))

cWspRef :: RuleRef
cWspRef = ref "c-wsp"

    -- , ("c-wsp", asDef $ asAlt (ref "WSP") <||> (group . asAlt $ asConcat (ref "c-nl") <.> (ref' "WSP")))

cWspRule :: RuleDecl
cWspRule = RuleDecl cWspRef (RuleDef $ (Core.wspRef ??) |! (group . asAlt $ cNlRef +? Core.wspRef)) 

cNlRef :: RuleRef
cNlRef = ref "c-nl"
    -- , ("c-nl", asDef $ asAlt (ref "comment") <||> (ref' "CRLF"))

cNlRule :: RuleDecl
cNlRule = RuleDecl cNlRef (RuleDef $ commentRef |? Core.crlfRef)

commentRef :: RuleRef
commentRef = ref "comment"
    -- , ("comment", asDef $ asConcat (strT ";") <.> (zeroOrMore . group $ (ref' "WSP") <||> (ref' "VCHAR")) <.> (ref' "CRLF"))

commentRule :: RuleDecl
commentRule = RuleDecl commentRef (RuleDef . asAlt $ (str ";") +! (zeroOrMore . group $ Core.wspRef |? Core.vcharRef) +? Core.crlfRef)

alternationRef :: RuleRef
alternationRef = ref "alternation"
    -- , ("alternation", asDef $ asConcat (ref "concatenation") <.> (zeroOrMore . group . asAlt $ (zeroOrMore $ ref' "c-wsp") <.> (str "/") <.> (zeroOrMore $ ref' "c-wsp") <.> (ref' "concatenation")))

alternationRule :: RuleDecl
alternationRule = RuleDecl alternationRef (RuleDef . asAlt $ (concatenationRef ??) +! (zeroOrMore . group . asAlt $ (zeroOrMore . asElem $ cWspRef) +! (str "/") +! (zeroOrMore . asElem $ cWspRef) +? concatenationRef))

concatenationRef :: RuleRef
concatenationRef = ref "concatenation"
    -- , ("concatenation", asDef $ asConcat (ref "repetition") <.> (zeroOrMore . group . asAlt $ (oneOrMore $ ref' "c-wsp") <.> (ref' "repetition")))

concatenationRule :: RuleDecl
concatenationRule = RuleDecl concatenationRef (RuleDef . asAlt $ (repetitionRef ??) +! (zeroOrMore . group . asAlt $ (oneOrMore . asElem $ cWspRef) +! (repetitionRef ??)))

repetitionRef :: RuleRef
repetitionRef = ref "repetition"

    -- , ("repetition", asDef $ asConcat (Opt . asAlt $ ref "repeat") <.> (ref' "element"))
repetitionRule :: RuleDecl
repetitionRule = RuleDecl repetitionRef $ (RuleDef . asAlt $ (opt (repeatRef ??)) +! (elementRef ??)) 

repeatRef :: RuleRef
repeatRef = ref "repeat"
    -- , ("repeat", asDef $ (oneOrMore $ ref' "DIGIT") <||> (group . asAlt $ (zeroOrMore $ ref' "DIGIT") <.> (str "*") <.> (zeroOrMore $ ref' "DIGIT")))

repeatRule :: RuleDecl
repeatRule = RuleDecl repeatRef (RuleDef $ (oneOrMore .asElem $ Core.digitRef) |! (group . asAlt $  (zeroOrMore . asElem $ Core.digitRef) +! (str "*") +! (zeroOrMore . asElem $ Core.digitRef)))

elementRef :: RuleRef
elementRef = ref "element"

    -- , ("element", asDef $ asAlt (ref "rulename") <||> (ref' "group") <||> (ref' "option") <||> (ref' "char-val") <||> (ref' "num-val") <||> (ref' "prose-val"))
elementRule :: RuleDecl
elementRule = RuleDecl elementRef (RuleDef $ rulenameRef |? groupRef |? optionRef |? charValRef |? numValRef |? proseValRef)

groupRef :: RuleRef
groupRef = ref "group"
    -- , ("group", asDef $ asConcat (strT "(") <.> (zeroOrMore $ ref' ("c-wsp")) <.> (ref' "alternation") <.> (zeroOrMore $ ref' ("c-wsp")) <.> (str ")"))

groupRule :: RuleDecl
groupRule = RuleDecl groupRef (RuleDef . asAlt $ (str "(") +! (zeroOrMore . asElem $ cWspRef) +! (alternationRef ??) +! (zeroOrMore . asElem $ cWspRef) +! (str ")"))

optionRef :: RuleRef
optionRef = ref "option"

    -- , ("option", asDef $ asConcat (strT "[") <.> (zeroOrMore $ ref' ("c-wsp")) <.> (ref' "alternation") <.> (zeroOrMore $ ref' ("c-wsp")) <.> (str "]"))
optionRule :: RuleDecl
optionRule = RuleDecl optionRef (RuleDef . asAlt $ (str "[") +! (zeroOrMore . asElem $ cWspRef) +? alternationRef +! (zeroOrMore . asElem $ cWspRef) +! (str "]"))

charValRef :: RuleRef
charValRef = ref "char-val"
    -- , ("char-val", asDef $ asConcat (ref "DQUOTE") <.> (zeroOrMore . group $ (rng 32 33) <||> (rng 34 126)) <.> (ref' "DQUOTE"))

charValRule :: RuleDecl
charValRule = RuleDecl charValRef (RuleDef . asAlt $ (Core.dquoteRef ??) +! (zeroOrMore . group $ (rng 32 33) |! (rng 34 126)) +? Core.dquoteRef)

numValRef :: RuleRef
numValRef = ref "num-val"
    -- , ("num-val", asDef $ asConcat (strT "%") <.> (group $ (ref' "bin-val") <||> (ref' "dec-val") <||> (ref' "hex-val")))

numValRule :: RuleDecl
numValRule = RuleDecl numValRef (RuleDef . asAlt $ (str "%") +! (group $ binValRef |? decValRef |? hexValRef))

binValRef :: RuleRef
binValRef = ref "bin-val"

binValRule :: RuleDecl
binValRule = RuleDecl binValRef (RuleDef . asAlt $ (str "b") +! (oneOrMore . asElem $ Core.bitRef) +! (opt $ (oneOrMore . group . asAlt $ (str ".") +! (oneOrMore . asElem $ Core.bitRef)) |! (group . asAlt $ (str "-") +! (oneOrMore . asElem $ Core.bitRef))))

decValRef :: RuleRef
decValRef = ref "dec-val"

decValRule :: RuleDecl
decValRule = RuleDecl decValRef (RuleDef . asAlt $ (str "d") +! (oneOrMore . asElem $ Core.digitRef) +! (opt $ (oneOrMore . group . asAlt $ (str ".") +! (oneOrMore . asElem $ Core.digitRef)) |! (group . asAlt $ (str "-") +! (oneOrMore . asElem $ Core.digitRef))))

hexValRef :: RuleRef
hexValRef = ref "hex-val"

hexValRule :: RuleDecl
hexValRule = RuleDecl hexValRef (RuleDef . asAlt $ (str "x") +! (oneOrMore . asElem $ Core.hexdigRef) +! (opt $ (oneOrMore . group . asAlt $ (str ".") +! (oneOrMore . asElem $ Core.hexdigRef)) |! (group . asAlt $ (str "-") +! (oneOrMore . asElem $ Core.hexdigRef))))

proseValRef :: RuleRef
proseValRef = ref "prose-val"

proseValRule :: RuleDecl
proseValRule = RuleDecl proseValRef (RuleDef . asAlt $ (str "<") +! (group $ (rng 32 61) |! (rng 63 126) ) +! (str ">"))