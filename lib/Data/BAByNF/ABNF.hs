{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
module Data.BAByNF.ABNF
  ( module Data.BAByNF.ABNF
  , module Data.BAByNF.ABNF.Model
  ) where

import Data.List qualified as List
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8

import Data.Attoparsec.ByteString qualified as Attoparsec.ByteString

import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Util.Binary qualified as Binary
import Data.BAByNF.Util.Decimal qualified as Decimal
import Data.BAByNF.Util.Hex qualified as Hex

import Data.BAByNF.Core.Ref (Ref)
import Data.BAByNF.Core.Ref qualified as Ref
import Data.BAByNF.Core.RefDict (RefDict (..))
import Data.BAByNF.Core.Parseable (Parseable)
import Data.BAByNF.Core.Parseable qualified as Parseable

import Data.BAByNF.Core.Repeat qualified as Core.Repeat

import Data.BAByNF.Core.Tree (Tree (..))
import Data.BAByNF.Core.Tree qualified as Tree
import Data.Functor ((<&>))
import Data.BAByNF.ABNF.Model

toRefDict :: Rulelist -> RefDict Rulename (Parseable Rulename)
toRefDict (Rulelist r) = RefDict (map (\(Rule ref _ (Elements a)) -> (ref, toParseable a)) r)


class ToParseable a where
  toParseable :: a -> Parseable Rulename

instance ToParseable Alternation where
  toParseable (Alternation x) = case x of
    [] -> error "empty alt"
    [z'] -> toParseable z'
    _:_ -> Parseable.Alt . List.NonEmpty.fromList . map toParseable $ x

instance ToParseable Concatenation where
  toParseable (Concatenation x) = case x of
    [] -> error "empty seq"
    [z'] -> toParseable z'
    _:_ -> Parseable.Seq . List.NonEmpty.fromList . map toParseable $ x

instance ToParseable Repetition where
  toParseable (Repetition r x) = 
    case r of 
      NoRepeat -> toParseable x 
      _ -> Parseable.Rep (toParseable x) (toRepeat r)
    where toRepeat NoRepeat = Core.Repeat.once
          toRepeat (FixedRepeat i) = Core.Repeat.exactly (fromInteger i)
          toRepeat (RangedRepeat lo hi) =
            let req = case lo of UnBound -> 0; (FixedBound l) -> fromInteger l
                opt = case hi of UnBound -> Nothing; (FixedBound h) -> if req > fromInteger h then error "fail" else Just $ fromInteger h - req
             in Core.Repeat.from req opt

instance ToParseable Element where
  toParseable e = case e of
    (RulenameElement r) -> toParseable r
    (GroupElement g) -> toParseable g
    (OptionElement o) -> toParseable o
    (CharValElement c) -> toParseable c
    (NumValElement n) -> toParseable n
    (ProseValElement p) -> toParseable p

instance ToParseable Rulename where
  toParseable = Parseable.Rule

instance ToParseable Group where
  toParseable (Group a) = toParseable a

instance ToParseable Option where
  toParseable (Option a) = Parseable.Rep (toParseable a) Core.Repeat.maybeOnce

instance ToParseable CharVal where
   toParseable charVal = case charVal of
    CaseInsensitiveCharVal ci -> toParseable ci
    CaseSensitiveCharVal cs -> toParseable cs

instance ToParseable CaseInsensitiveString where
  toParseable (CaseInsensitiveString x@(QuotedString b)) = Parseable.Unit (prettyPrint x) (Ascii.parseCaseInsensitive b <&> (\b' -> (Tree [Tree.StringNode b'])))

instance ToParseable CaseSensitiveString where
  toParseable (CaseSensitiveString x@(QuotedString b)) = Parseable.Unit (prettyPrint x) (Ascii.parseCaseSensitive b <&> (\b' -> (Tree [Tree.StringNode b'])))

instance ToParseable ProseVal where
  toParseable x = Parseable.Unit (prettyPrint x) (fail "prose-val not supported yet")


instance ToParseable NumVal where
  toParseable numVal = case numVal of
    BinNumVal b -> toParseable b
    DecNumVal d -> toParseable d
    HexNumVal x -> toParseable x

instance ToParseable BinVal where
  toParseable x = case x of
    SeqBinVal s -> Parseable.Unit (prettyPrint x)  $ Attoparsec.ByteString.string (ByteString.pack $ map Binary.toNum s) <&> (\b' -> Tree [Tree.StringNode b'])
    RangeBinVal lo hi -> Parseable.Unit (prettyPrint x) $ Attoparsec.ByteString.satisfy (\w -> w >= Binary.toNum lo && w <= Binary.toNum hi) <&> \w -> Tree [Tree.StringNode (ByteString.singleton w)]

instance ToParseable DecVal where
  toParseable x = case x of
    SeqDecVal s -> Parseable.Unit (prettyPrint x) $ Attoparsec.ByteString.string (ByteString.pack $ map Decimal.toNum s) <&> (\b' -> Tree [Tree.StringNode b'])
    RangeDecVal lo hi -> Parseable.Unit (prettyPrint x) $ Attoparsec.ByteString.satisfy (\w -> w >= Decimal.toNum lo && w <= Decimal.toNum hi) <&> \w -> Tree [Tree.StringNode (ByteString.singleton w)]

instance ToParseable HexVal where
  toParseable x = case x of
    SeqHexVal s -> Parseable.Unit (prettyPrint x) $ Attoparsec.ByteString.string (ByteString.pack $ map Hex.toNum s) <&> (\b' -> Tree [Tree.StringNode b'])
    RangeHexVal lo hi -> Parseable.Unit (prettyPrint x) $ Attoparsec.ByteString.satisfy (\w -> w >= Hex.toNum lo && w <= Hex.toNum hi) <&> \w -> Tree [Tree.StringNode (ByteString.singleton w)]

class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint Rulelist where
  prettyPrint (Rulelist x) = concatMap prettyPrint x
instance Show Rulelist where
  show x = "Rulelist{" ++ prettyPrint x ++ "}"

instance PrettyPrint Rule where
  prettyPrint (Rule rulename definedAs elements) = prettyPrint rulename ++ " " ++ prettyPrint definedAs ++ " " ++ (prettyPrint elements)

instance Show Rule where 
  show x = "Rule{" ++ prettyPrint x ++ "}"
instance PrettyPrint Rulename where
  prettyPrint (Rulename x) = ByteString.Char8.unpack x

instance Show Rulename where
  show x = "Rulename{" ++ prettyPrint x ++ "}"

instance PrettyPrint DefinedAs where
  prettyPrint BasicDefinition = "="
  prettyPrint IncrementalAlternative = "=/"

instance Show DefinedAs where
  show x = "DefinedAs{" ++ prettyPrint x ++ "}" 

instance PrettyPrint Elements where
  prettyPrint (Elements a) = prettyPrint a

instance Show Elements where
  show x = "Elements{" ++ prettyPrint x ++ "}"

instance PrettyPrint Alternation where
  prettyPrint (Alternation x) = List.intercalate " / " (map prettyPrint x)

instance Show Alternation where
  show x = "Alternation{" ++ prettyPrint x ++ "}"

instance PrettyPrint Concatenation where
  prettyPrint (Concatenation x) = List.intercalate " " (map prettyPrint x)

instance Show Concatenation where
  show x = "Concatenation{" ++ prettyPrint x ++ "}"

instance PrettyPrint Repetition where
  prettyPrint (Repetition r e) = prettyPrint r ++ prettyPrint e

instance Show Repetition where
  show x = "Repetition{" ++ prettyPrint x ++ "}"
instance PrettyPrint Repeat where
  prettyPrint NoRepeat = ""
  prettyPrint (FixedRepeat i) = show i
  prettyPrint (RangedRepeat mn mx) = toString mn ++ "*" ++ toString mx
    where toString UnBound = ""
          toString (FixedBound i) = show i

instance Show Repeat where
  show x = "Repeat{" ++ prettyPrint x ++ "}"

instance PrettyPrint Element where
  prettyPrint (RulenameElement x) = prettyPrint x
  prettyPrint (GroupElement x) = prettyPrint x
  prettyPrint (OptionElement x) = prettyPrint x
  prettyPrint (NumValElement x) = prettyPrint x
  prettyPrint (CharValElement x) = prettyPrint x
  prettyPrint (ProseValElement x) = prettyPrint x

instance Show Element where
  show x = "Element{" ++ prettyPrint x ++ "}"

instance PrettyPrint Group where
  prettyPrint (Group x) = "(" ++ prettyPrint x ++ ")"

instance Show Group where
  show x = "Group{" ++ prettyPrint x ++ "}"

instance PrettyPrint Option where
  prettyPrint (Option x) = "[" ++ prettyPrint x ++ "]"

instance Show Option where
  show x = "Option{" ++ prettyPrint x ++ "}"

instance PrettyPrint NumVal where
  prettyPrint x = "%" ++
    ( case x of
      BinNumVal y -> prettyPrint y
      DecNumVal y -> prettyPrint y
      HexNumVal y -> prettyPrint y
    )

instance Show NumVal where
  show x = "NumVal{" ++ prettyPrint x ++ "}"

instance PrettyPrint BinVal where
  prettyPrint x = "b" ++
    (case x of
      SeqBinVal y -> List.intercalate "." (map show y)
      RangeBinVal y z -> show y ++ "-" ++ show z
    )

instance Show BinVal where
  show x = "BinVal{" ++ prettyPrint x ++ "}"

instance PrettyPrint DecVal where
  prettyPrint x = "d" ++
    (case x of
      SeqDecVal y -> List.intercalate "." (map show y)
      RangeDecVal y z -> show y ++ "-" ++ show z
    )

instance Show DecVal where
  show x = "DecVal{" ++ prettyPrint x ++ "}"


instance PrettyPrint HexVal where
  prettyPrint x = "x" ++
    (case x of
      SeqHexVal y -> List.intercalate "." (map show y)
      RangeHexVal y z -> show y ++ "-" ++ show z
    )

instance Show HexVal where
  show x = "HexVal{" ++ prettyPrint x ++ "}"

instance PrettyPrint CharVal where
  prettyPrint (CaseInsensitiveCharVal x) = prettyPrint x
  prettyPrint (CaseSensitiveCharVal x) = prettyPrint x

instance Show CharVal where
  show x = "CharVal{" ++ prettyPrint x ++ "}"

instance PrettyPrint CaseInsensitiveString where
  prettyPrint (CaseInsensitiveString x) = prettyPrint x

instance Show CaseInsensitiveString where
  show x = "CaseInsensitiveString{" ++ prettyPrint x ++ "}"

instance PrettyPrint CaseSensitiveString where
  prettyPrint (CaseSensitiveString x) = "%s" ++ prettyPrint x

instance Show CaseSensitiveString where
  show x = "CaseSensitiveString{" ++ prettyPrint x ++ "}"

instance PrettyPrint QuotedString where
  prettyPrint (QuotedString b) = "\"" ++ ByteString.Char8.unpack b ++ "\""

instance Show QuotedString where
  show x = "QuotedString{" ++ prettyPrint x ++ "}"

instance PrettyPrint ProseVal where
  prettyPrint (ProseVal b) = "<" ++ ByteString.Char8.unpack b ++ ">"

instance Show ProseVal where
  show x = "ProseVal{" ++ prettyPrint x ++ "}"
