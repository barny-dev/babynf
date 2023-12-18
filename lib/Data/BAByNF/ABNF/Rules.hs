module Data.BAByNF.ABNF.Rules where

import Data.Bits qualified as Bits
import Data.Word (Word8)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Data.Maybe qualified as Maybe
import Data.Functor ((<&>))


import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.BAByNF.ABNF.Grammar
import Data.BAByNF.ABNF.Core qualified as Core
import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.Util.Stream (Stream)
import Data.BAByNF.Util.Stream qualified as Stream

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

grammarFromTree :: Tree ABNFRef -> Either [String] Grammar
grammarFromTree tree =
    let errorsAndDecls = groupBySide $ map ruleDeclFromTree $ Tree.getChildrenWithRef (toRef ruleRef) tree
     in case errorsAndDecls of
        (err:errors, _) -> Left $ err : errors
        ([], decls) -> Right $ Grammar decls


groupBySide :: [Either l r] -> ([l], [r])
groupBySide = foldr (\lr (ls, rs) -> case lr of Left l -> (l:ls, rs); Right r -> (ls, r:rs)) ([], [])


ruleDeclFromTree :: Tree ABNFRef -> Either String RuleDecl
ruleDeclFromTree tree =
    rr >>= \rr' ->
    alts >>= \alts' ->
    return $ RuleDecl rr' (RuleDef alts')
    where rr = Tree.tryGetChildWithRef (toRef rulenameRef) tree <&> ruleRefFromTree
          alts = Tree.tryGetFirstPath (toRef elementsRef :| [toRef alternationRef]) tree >>= altFromTree


ruleRef :: RuleRef
ruleRef = ref "rule"

ruleRule :: RuleDecl
ruleRule = RuleDecl ruleRef (RuleDef . asAlt $ rulenameRef +? definedAsRef +? elementsRef +? cNlRef)

rulenameRef :: RuleRef
rulenameRef = ref "rulename"

    -- , ("rulename", asDef $ (ref' "ALPHA") <.> (zeroOrMore . group $ (asAlt (ref "ALPHA") <||> (ref' "DIGIT") <||> (str "-"))))

rulenameRule :: RuleDecl
rulenameRule = RuleDecl rulenameRef (RuleDef . asAlt $ (Core.alphaRef ??) +! (zeroOrMore . group $ Core.alphaRef |? Core.digitRef |! (str "-")))

ruleRefFromTree :: Tree ABNFRef -> RuleRef
ruleRefFromTree = RuleRef . Tree.stringify

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

altFromTree :: Tree ABNFRef -> Either String Alt
altFromTree tree =
    let concatTrees = Tree.getChildrenWithRef (toRef concatenationRef) tree
     in mapM concatFromTree concatTrees >>= \t -> case t of [] -> Left "empty alt"; x:xs -> Right $ Alt (x :| xs)

concatenationRef :: RuleRef
concatenationRef = ref "concatenation"

concatenationRule :: RuleDecl
concatenationRule = RuleDecl concatenationRef (RuleDef . asAlt $ (repetitionRef ??) +! (zeroOrMore . group . asAlt $ (oneOrMore . asElem $ cWspRef) +! (repetitionRef ??)))

concatFromTree :: Tree ABNFRef -> Either String Concat
concatFromTree tree = mapM repFromTree (Tree.getChildrenWithRef (toRef repetitionRef) tree) >>= \t -> case t of [] -> Left "empty concat"; x:xs -> Right $ Concat (x :| xs)

repetitionRef :: RuleRef
repetitionRef = ref "repetition"

repetitionRule :: RuleDecl
repetitionRule = RuleDecl repetitionRef (RuleDef . asAlt $ (opt (repeatRef ??)) +! (elementRef ??))

repFromTree :: Tree ABNFRef -> Either String Rep
repFromTree tree =
    let rd = maybe (Right Nothing) (fmap Just . repeatFromTree)  (Tree.getChildWithRef (toRef repeatRef) tree)
        e = Tree.tryGetChildWithRef (toRef elementRef) tree >>= elemFromTree
     in rd >>= \rd' ->
         e >>= \e' -> return $ Rep rd' e'

repeatFromTree :: Tree ABNFRef -> Either String RepDef
repeatFromTree tree = -- takeWhile DIGIT takeIf "*" takeWhile DIGIT  
    let stream = do
            mnOpt <- takeDigits
            hasStar <- Stream.take <&> Maybe.isJust
            mxOpt <- if hasStar then takeDigits else return Nothing
            case (mnOpt, hasStar, mxOpt) of
                (Just mns, _, Nothing) -> return $ tryToInteger mns >>= \mn -> return $ RepDef (Just mn) (if hasStar then Nothing else Just mn)
                (Just mns, True, Just mxs) -> return $ tryToInteger mns >>= \mn -> tryToInteger mxs >>= \mx -> return $ RepDef (Just mn) (Just mx)
                (Nothing, True, Just mxs) -> return $ tryToInteger mxs >>= \mx -> return $ RepDef Nothing (Just mx)
                _ -> return $ Left "illegal state"
     in Stream.runStream_ stream (Tree.nodes tree)
    where takeDigits = Stream.takeWhileMap (\e ->
            case e of
                Tree.RefNode r subtree ->
                    if r == (toRef Core.digitRef)
                        then Just $ Tree.stringify subtree
                        else Nothing
                _ -> Nothing
            ) <&> \bs -> case bs of [] -> Nothing; _ -> Just . ByteString.concat $ bs
          tryToInteger bs =
              case ByteString.Char8.readInteger bs of
                  Nothing -> Left "not integer"
                  Just (no, rest) | ByteString.null rest  -> Left "more than an integer read"
                                  | otherwise -> Right no


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

elemFromTree :: Tree ABNFRef -> Either String Elem
elemFromTree tree = do Stream.runStream_ stream (Tree.nodes tree)
    where stream = do
            nodeOrErr <- Stream.take <&> maybe (Left "no elem contained") Right
            return $ nodeOrErr >>= \node -> case node of
                Tree.StringNode _ -> Left "ref node expected"
                Tree.RefNode ref subtree | ref == (toRef rulenameRef) -> Right . RuleRefE . ruleRefFromTree $ subtree
                                         | ref == (toRef groupRef) ->  groupFromTree subtree <&> GroupE
                                         | ref == (toRef optionRef) -> optFromTree subtree <&> OptE
                                         | ref == (toRef charValRef) -> charTermFromTree subtree <&> TermE
                                         | ref == (toRef numValRef) -> numTermFromTree subtree <&> TermE
                                         | ref == (toRef proseValRef) -> proseFromTree subtree <&> ProseE
                                         | otherwise -> Left $ "unexpected ref <" ++ show ref ++ ">"

groupFromTree :: Tree ABNFRef -> Either String Group
groupFromTree tree = altFromTree tree <&> Group

optFromTree :: Tree ABNFRef -> Either String Opt
optFromTree tree = altFromTree tree <&> Opt

charTermFromTree :: Tree ABNFRef -> Either String Term -- TODO: support RFC 7405
charTermFromTree tree =
    maybe (Left "char-val must be between \" and \"") Right $
        unconsnoc (Tree.stringify tree) >>= \(h, m, l) ->
            if h == 34 && l == 34
                then Just (ArrayTerm CaseInsensitive m)
                else Nothing

numTermFromTree :: Tree ABNFRef -> Either String Term
numTermFromTree tree = case Tree.nodes tree of
    [Tree.StringNode b, Tree.RefNode ref subtree] ->
        if b /= ByteString.singleton 37
            then Left "expected % prefix"
            else if ref == (toRef binValRef) then binNumTermFromTree subtree
                else if ref == (toRef decValRef) then decNumTermFromTree subtree
                else if ref == (toRef hexValRef) then hexNumTermFromTree subtree
                else Left "unexpected rule ref"
    _ -> Left "exactly two nodes expected - \"%\" and bin-val | dec-val | hex-val"

decNumTermFromTree :: Tree ABNFRef -> Either String Term
decNumTermFromTree tree = undefined

hexNumTermFromTree :: Tree ABNFRef -> Either String Term
hexNumTermFromTree tree = undefined

unconsnoc :: ByteString -> Maybe (Word8, ByteString, Word8)
unconsnoc bs = ByteString.uncons bs >>= \(h, t) -> ByteString.unsnoc t <&> \(m, l) -> (h, m, l)

proseFromTree :: Tree ABNFRef -> Either String Prose
proseFromTree tree =
    let whole = Tree.stringify tree
        proseOrErr = Maybe.fromMaybe (Left "prose must be between < and >") $ ByteString.uncons whole >>= \(h, t) ->
             ByteString.unsnoc t <&> \(prose', l) ->
                if h == 60 && l == 62 then Right prose' else Left "prose must be between < and >"
     in proseOrErr <&> Prose

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

binNumTermFromTree :: Tree ABNFRef -> Either String Term
binNumTermFromTree tree = Stream.runStream_ stream (Tree.nodes tree)
    where stream :: Stream (Tree.Node ABNFRef) (Either String Term)
          stream = ( Stream.takeIf (\node ->
                        Tree.isStringEq node (ByteString.singleton 98) ||
                        Tree.isStringEq node (ByteString.singleton 66))
                     <&> \m -> case m of Nothing -> Left "expected b or B"; Just _ -> Right ()
                   ) `Stream.either'` const (
                        Stream.takeWhile (\n -> Tree.isRefOf n (toRef Core.bitRef)) >>=
                             \l -> return $ case l of [] -> Left "no bits"; _ -> Right $ ByteString.concat . map Tree.stringifyNode $ l)
                     `Stream.either'` (\bits -> return (bitsToW8 bits)) >>=
                                      (\startVal -> Stream.peek) >>= 
                                      (\nextNode-> case nextNode of
                                            Nothing -> return $ Right (ArrayTerm CaseInsensitive (ByteString.singleton startVal))
                                            Just Tree.StringNode s | s == ByteString.singleton )
            -- undefined

bitsToW8 :: ByteString -> Either String Word8
bitsToW8 bs
    | ByteString.null bs = Left "no bits"
    | otherwise = bitsToW8' bs 0 0
    where bitsToW8' :: ByteString -> Int -> Word8 -> Either String Word8
          bitsToW8' b digits val =
            case ByteString.uncons b of
                Nothing | digits < 1-> Left "minimum 1 binary digit required"
                        | digits > 8 -> Left "maximum 8 binary digits"
                        | otherwise -> Right val
                Just (h, r) | h == 48 -> bitsToW8' r (digits + 1) (Bits.shiftL val 1)
                            | h == 49 -> bitsToW8' r (digits + 1) (Bits.shiftL val 1 + 1)
                            | otherwise -> Left "expected 0 or 1"


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
proseValRule = RuleDecl proseValRef (RuleDef . asAlt $ (str "<") +! (zeroOrMore . group $ (rng 32 61) |! (rng 63 126) ) +! (str ">"))