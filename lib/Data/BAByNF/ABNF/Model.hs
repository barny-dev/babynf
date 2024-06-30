module Data.BAByNF.ABNF.Model where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString

import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Util.Binary qualified as Binary
import Data.BAByNF.Util.Decimal qualified as Decimal
import Data.BAByNF.Util.Hex qualified as Hex

import Data.BAByNF.Core.Ref (Ref)
import Data.BAByNF.Core.Ref qualified as Ref

newtype Rulelist = Rulelist [Rule]

data DefinedAs = BasicDefinition | IncrementalAlternative

data Rule = Rule Rulename DefinedAs Elements

newtype Rulename = Rulename ByteString deriving Eq

instance Ref Rulename where
  eq (Rulename x) (Rulename y) = Ascii.eqNoCaseBS x y
  display (Rulename x) = show x

newtype Elements = Elements Alternation
newtype Alternation = Alternation [Concatenation] deriving Eq

newtype Concatenation = Concatenation [Repetition] deriving Eq

data Repetition = Repetition Repeat Element deriving Eq

data Repeat = NoRepeat | FixedRepeat Integer | RangedRepeat Bound Bound deriving Eq
data Bound = UnBound | FixedBound Integer deriving Eq

data Element = RulenameElement Rulename
             | GroupElement Group
             | OptionElement Option
             | CharValElement CharVal
             | NumValElement NumVal
             | ProseValElement ProseVal
             deriving Eq


newtype Group = Group Alternation deriving Eq

newtype Option = Option Alternation deriving Eq

data CharVal = CaseInsensitiveCharVal CaseInsensitiveString
             | CaseSensitiveCharVal CaseSensitiveString 
             deriving Eq

newtype CaseInsensitiveString = CaseInsensitiveString QuotedString deriving Eq

newtype CaseSensitiveString = CaseSensitiveString QuotedString deriving Eq

newtype QuotedString = QuotedString ByteString deriving Eq

data NumVal = BinNumVal BinVal
            | DecNumVal DecVal
            | HexNumVal HexVal
            deriving Eq


data BinVal = SeqBinVal [Binary.Seq] | RangeBinVal Binary.Seq Binary.Seq deriving Eq

data DecVal = SeqDecVal [Decimal.Seq] | RangeDecVal Decimal.Seq Decimal.Seq deriving Eq

data HexVal = SeqHexVal [Hex.Seq] | RangeHexVal Hex.Seq Hex.Seq deriving Eq

newtype ProseVal = ProseVal ByteString deriving Eq
