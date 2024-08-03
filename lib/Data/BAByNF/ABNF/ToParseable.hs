module Data.BAByNF.ABNF.ToParseable
    ( ToParseable
    , toParseable
    ) where

import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as List.NonEmpty

import Data.ByteString qualified as ByteString

import Data.Attoparsec.ByteString qualified as Attoparsec.ByteString

import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Util.Binary qualified as Binary
import Data.BAByNF.Util.Decimal qualified as Decimal
import Data.BAByNF.Util.Hex qualified as Hex
import Data.BAByNF.Core.Parseable (Parseable)
import Data.BAByNF.Core.Parseable qualified as Parseable
import Data.BAByNF.Core.Repeat qualified as Core.Repeat
import Data.BAByNF.Core.Tree (Tree (..))
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF.Model
import Data.BAByNF.ABNF.PrettyPrint

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

