{-# LANGUAGE UndecidableInstances #-}
module Data.BAByNF.ABNF.Grammar where

import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.String ( IsString(fromString) )
import Data.Word
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import qualified Data.List.NonEmpty as NonEmpty

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8 ()
import Data.Attoparsec.ByteString as Attoparsec
import Numeric.Natural

import Data.BAByNF.Util.Ascii qualified as Ascii

import Data.BAByNF.Core.Parseable (Parseable)
import Data.BAByNF.Core.Parseable qualified as Parseable
import Data.BAByNF.Core.Ref (Ref, CaseInsensitiveAsciiRef)
import Data.BAByNF.Core.Ref qualified as Ref
import Data.BAByNF.Core.RefDict (RefDict (..))
import Data.BAByNF.Core.RefDict qualified as RefDict
import Data.BAByNF.Core.Repeat (Repeat)
import Data.BAByNF.Core.Repeat qualified as Repeat
import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree



type ABNFRef = CaseInsensitiveAsciiRef
type ABNFParseable = Parseable ABNFRef
type ABNFDict = RefDict ABNFRef ABNFParseable
data Grammar = Grammar [RuleDecl] deriving (Eq, Show)
empty = Grammar []

toDict :: Grammar -> ABNFDict
toDict (Grammar declarations) =
    RefDict $ map toEntry declarations
    where toEntry (RuleDecl ruleRef (RuleDef alt)) = (toRef ruleRef, altParser alt) 

toRef :: RuleRef -> ABNFRef
toRef (RuleRef bs) = Ref.caseInsensitiveAscii bs

toParser :: Grammar -> RuleRef -> Parseable.TreeParser ABNFRef
toParser grammar ruleRef = Parseable.toParser (toDict grammar) (Parseable.Rule (toRef ruleRef))

data RuleDecl = RuleDecl RuleRef RuleDef deriving (Eq, Show)

data RuleDef = RuleDef Alt deriving (Eq, Show)

data RuleRef = RuleRef BS.ByteString deriving Show

instance Eq RuleRef where
  (==) (RuleRef a) (RuleRef b) = Ascii.eqNoCaseBS a b

data Elem = RuleRefE RuleRef | GroupE Group | OptE Opt | TermE Term | ProseE Prose deriving (Eq, Show)

data Group = Group Alt deriving (Eq, Show)

data Alt = Alt (NonEmpty Concat) deriving (Eq, Show)

instance Semigroup Alt where
  (<>) (Alt x1) (Alt x2) = Alt $ x1 <> x2

data Concat = Concat (NonEmpty Rep) deriving (Eq, Show)

instance Semigroup Concat where
  (<>) (Concat x1) (Concat x2) = Concat $ x1 <> x2

data Rep = Rep (Maybe RepDef) Elem deriving (Eq, Show)
data RepDef = RepDef (Maybe Integer) (Maybe Integer) deriving (Eq, Show)

data Opt = Opt Alt deriving (Eq, Show)

class AsElem a where
  asElem :: a -> Elem

instance AsElem Elem where
  asElem = id

instance AsElem RuleRef where
  asElem = RuleRefE

instance AsElem Group where
  asElem = GroupE

instance AsElem Opt where
  asElem = OptE

instance AsElem Term where
  asElem = TermE

instance AsElem Prose where
  asElem = ProseE

data CaseMode = CaseInsensitive | CaseSensitive deriving (Eq, Show)
data Term = ArrayTerm CaseMode BS.ByteString | RangeTerm Word8 Word8 deriving Show

data Prose = Prose BS.ByteString deriving (Eq, Show)

instance Eq Term where
  (==) (ArrayTerm CaseInsensitive a) (ArrayTerm CaseInsensitive b) = Ascii.eqNoCaseBS a b
  (==) (ArrayTerm CaseSensitive a) (ArrayTerm CaseSensitive b) = a == b
  (==) (RangeTerm lo1 hi1) (RangeTerm lo2 hi2) = lo1 == lo2 && hi1 == hi2
  (==) _ _ = False

getDecls :: Grammar -> RuleRef -> [RuleDecl]
getDecls (Grammar decls) ref =
  filter (\(RuleDecl ref' _) -> ref == ref') decls

getMergedDecl :: Grammar -> RuleRef -> Maybe RuleDecl
getMergedDecl grammar ref =
  let decls = getDecls grammar ref
      fromDecl (RuleDecl _ (RuleDef (Alt x))) = x
      toDecl x = RuleDecl ref (RuleDef (Alt x))
      maybeNonEmpty = NonEmpty.nonEmpty $ foldMap (NonEmpty.toList . fromDecl) decls
  in fmap toDecl maybeNonEmpty

coreRules :: [RuleDecl]
coreRules = map (uncurry define)
    [ ("ALPHA", oneOfTwoT (RangeTerm 65 90) (RangeTerm 97 122))
    , ("BIT", oneOfTwoT (strT "0") (strT "1"))
    , ("CHAR", singleT $ RangeTerm 1 127)
    , ("CR", singleT $ byteT 13)
    , ("CRLF", exactly $ concatRefs ["CR", "LF"])
    , ("CTL", oneOfTwoT (RangeTerm 0 31) (byteT 127))
    , ("DIGIT", singleT $ RangeTerm 48 57)
    , ("DQUOTE", singleT $ byteT 34)
    , ("HEXDIG", Alt $ (concatRefs ["DIGIT"]) :| (map (only . once . TermE . strT) ["A", "B", "C", "D", "E", "F"]))
    , ("HTAB", singleT $ byteT 9)
    , ("LF", singleT $ byteT 10)
    , ("LWSP", (zeroOrMore :: Elem -> Alt) . group $ (ref' "WSP") <||> (asAlt $ (ref' "CRLF") <.> (ref' "WSP")))
    , ("OCTET", singleT (RangeTerm 0 255))
    , ("SP", singleT $ byteT 32)
    , ("VCHAR", singleT $ RangeTerm 33 126)
    , ("WSP", Alt (concatRefs ["SP"] :| [concatRefs ["HTAB"]]))
    ]
    where define r def = RuleDecl (ref r) (RuleDef def)
          singleT term = exactly . only . once $ TermE term
          oneOfTwoT term1 term2 = Alt $ (only . once $ TermE term1) :| (only . once $ TermE term2) : []
          concatRefs x = Concat (NonEmpty.fromList . (map (once . RuleRefE . ref)) $ x)



define :: String -> Alt -> RuleDecl
define r def = RuleDecl (ref r) (RuleDef def)

ref :: String -> RuleRef
ref s = RuleRef $ fromString s

exactly :: Concat -> Alt
exactly x = Alt $ x :| []
only :: Rep -> Concat
only x = Concat $ x :| []
once :: Elem -> Rep
once x = Rep Nothing x
only1 :: Elem -> Concat
only1 x = only . once $ x


byte :: (FromElem a) => Word8 -> a
byte = fromElem . TermE . (ArrayTerm CaseSensitive) . BS.singleton
str :: (FromElem a) => String -> a
str = fromElem . TermE . (ArrayTerm CaseInsensitive) . fromString
str' :: (FromElem a) => String -> a
str' = fromElem . TermE . (ArrayTerm CaseSensitive) . fromString
rng :: (FromElem a) => Word8 -> Word8 -> a
rng mn mx = fromElem . TermE $ RangeTerm mn mx
opt :: (FromElem a) => Alt -> a
opt = fromElem . OptE . Opt
ref' :: (FromElem a) => String -> a
ref' = fromElem . RuleRefE . ref

byteT x = ArrayTerm CaseSensitive $ BS.singleton x
strT x = ArrayTerm CaseInsensitive $ fromString x

zeroOrMore :: (FromRep a) => Elem -> a
zeroOrMore = fromRep . (Rep (Just $ RepDef Nothing Nothing))
oneOrMore :: (FromRep a) => Elem -> a
oneOrMore = fromRep . (Rep (Just $ RepDef (Just 1) Nothing))

groupE :: Alt -> Elem
groupE x = GroupE . Group $ x
group :: (FromElem a) => Alt -> a
group = fromElem . GroupE . Group

orC :: Concat -> Concat -> Alt
orC a b = Alt $ a :| b : []

class AsDef a where
  asDef :: a -> RuleDef

instance AsDef Alt where
  asDef = RuleDef

instance AsDef Concat where
  asDef = RuleDef . asAlt

instance AsDef Rep where
  asDef = RuleDef . asAlt

instance AsDef Elem where
  asDef = RuleDef . asAlt

(<.>) :: Concat -> Concat -> Concat
(<.>) c x = c <> x
(+!) :: Concat -> Concat -> Concat
(+!) = (<>)
(+?) :: (AsConcat a, AsConcat b) => a -> b -> Concat
(+?) a b = asConcat a <> asConcat b

(<..>) :: (AsConcat a, AsConcat b) => a -> b -> Concat
(<..>) a b = asConcat a <.> asConcat b

(|!) :: Alt -> Alt -> Alt
(|!) = (<>)

(|?) :: (AsAlt a, AsAlt b) => a -> b -> Alt
(|?) a b = asAlt a <> asAlt b

(??) :: (AsElem a, FromElem b) => a -> b
(??) = fromElem . asElem


(<||>) :: Alt -> Alt -> Alt
(<||>) a x = a <> x
(<|||>) :: (AsAlt a, AsAlt b) => a -> b -> Alt
(<|||>) a b = asAlt a <||> asAlt b



class FromElem a where
  fromElem :: Elem -> a
instance FromElem Elem where
  fromElem = id
instance FromElem Rep where
  fromElem = Rep Nothing
instance FromElem Concat where
  fromElem = asConcat

instance FromElem Alt where
  fromElem = asAlt

instance FromElem RuleDef where
  fromElem = asDef

class FromRep a where
  fromRep :: Rep -> a

instance FromRep Concat where
  fromRep = asConcat

instance FromRep Alt where
  fromRep = asAlt

instance FromRep RuleDef where
  fromRep = asDef

class AsAlt a where
  asAlt :: a -> Alt

instance AsAlt Alt where
  asAlt = id

instance AsAlt Concat where
  asAlt = Alt . NonEmpty.singleton

instance AsAlt Rep where
  asAlt = Alt . NonEmpty.singleton . asConcat

instance AsAlt Elem where
  asAlt = Alt . NonEmpty.singleton . asConcat

instance AsAlt Group where
  asAlt = asAlt . GroupE

instance AsAlt Opt where
  asAlt = asAlt . OptE

instance AsAlt Term where
  asAlt = asAlt . TermE

instance AsAlt RuleRef where
  asAlt = asAlt . RuleRefE

class AsConcat a where
  asConcat :: a -> Concat

instance AsConcat Concat where
  asConcat = id

instance AsConcat Rep where
  asConcat = Concat . NonEmpty.singleton

instance AsConcat Elem where
  asConcat x = Concat . NonEmpty.singleton $ Rep Nothing x

instance AsConcat Group where
  asConcat = asConcat . GroupE

instance AsConcat Opt where
  asConcat = asConcat . OptE

instance AsConcat Term where
  asConcat = asConcat . TermE

instance AsConcat RuleRef where
  asConcat = asConcat . RuleRefE

ruleToParseable grammar ref =
    case getMergedDecl grammar ref of
        Nothing -> error $ "non-existing rule " ++ show ref
        Just (RuleDecl _ (RuleDef alt)) -> altParser alt

altParser :: Alt -> ABNFParseable
altParser (Alt nonEmpty) =
    case NonEmpty.map concatParser nonEmpty of
        x :| [] -> x
        x :| xs -> Parseable.Alt (x :| xs)

concatParser :: Concat -> ABNFParseable
concatParser (Concat nonEmpty) =
    case NonEmpty.map repParser nonEmpty of
        x :| [] -> x
        x :| xs -> Parseable.Seq (x :| xs)

repParser :: Rep -> ABNFParseable
repParser (Rep maybeRepDef e) = case maybeRepDef of
    Nothing -> elemParser e
    Just (RepDef maybeLo maybeHi) ->
      let required = (fromInteger $ fromMaybe 0 maybeLo) :: Natural
          optional = fmap (\hi -> fromInteger hi - required) maybeHi
       in Parseable.Rep (elemParser e) (Repeat.from required optional)

elemParser :: Elem -> ABNFParseable
elemParser def =
    case def of
        RuleRefE ref -> ruleParser ref
        GroupE group -> groupParser group
        OptE opt -> optParser opt
        TermE term -> termParser term


groupParser :: Group -> ABNFParseable
groupParser (Group alt) = altParser alt

optParser :: Opt -> ABNFParseable
optParser (Opt alt) = Parseable.Rep (altParser alt) Repeat.maybeOnce

termParser :: Term -> ABNFParseable
termParser def = Parseable.Unit (friendlyShow def) (parser def <&> toTree)
    where parser (RangeTerm lo hi) = Attoparsec.satisfy (\x -> x >= lo && x <= hi) <&> BS.singleton
          parser (ArrayTerm CaseSensitive bs) = Attoparsec.string bs
          parser (ArrayTerm CaseInsensitive bs) =
                  Attoparsec.take (BS.length bs) >>=
                  (\bs' -> if Ascii.eqNoCaseBS bs' bs
                      then return bs'
                      else fail "Term not matched")
          toTree = Tree.singleton . Tree.StringNode

-- TODO: implement to match ABNF notation
friendlyShow :: Term -> String
friendlyShow = show

ruleParser :: RuleRef -> ABNFParseable
ruleParser (RuleRef bs) = Parseable.Rule $ Ref.caseInsensitiveAscii bs