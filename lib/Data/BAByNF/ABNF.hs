{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.BAByNF.ABNF where

import Data.ByteString (ByteString)

import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Util.Binary qualified as Binary
import Data.BAByNF.Util.Decimal qualified as Decimal
import Data.BAByNF.Util.Hex qualified as Hex

import Data.BAByNF.Core.Ref (Ref)
import Data.BAByNF.Core.Ref qualified as Ref

newtype Rulelist = Rulelist [Rule]

data DefinedAs = BasicDefinition | IncrementalAlternative

data Rule = Rule Rulename DefinedAs Elements

newtype Rulename = Rulename ByteString

instance Ref Rulename where 
  eq (Rulename x) (Rulename y) = Ascii.eqNoCaseBS x y
  display (Rulename x) = show x

newtype Elements = Elements Alternation
newtype Alternation = Alternation [Concatenation]

newtype Concatenation = Concatenation [Repetition]
 
data Repetition = Repetition Repeat Element

data Repeat = NoRepeat | FixedRepeat Integer | RangedRepeat Bound Bound
data Bound = UnBound | FixedBound Integer

data Element = RulenameElement Rulename
             | GroupElement Group
             | OptionElement Option
             | CharValElement CharVal
             | NumValElement NumVal
             | ProseValElement ProseVal


newtype Group = Group Alternation

newtype Option = Option Alternation

data CharVal = CaseInsensitiveCharVal CaseInsensitiveString
             | CaseSensitiveCharVal CaseSensitiveString

newtype CaseInsensitiveString = CaseInsensitiveString QuotedString

newtype CaseSensitiveString = CaseSensitiveString QuotedString

newtype QuotedString = QuotedString ByteString

data NumVal = BinNumVal BinVal
            | DecNumVal DecVal
            | HexNumVal HexVal


data BinVal = SeqBinVal [Binary.Seq] | RangeBinVal Binary.Seq Binary.Seq

data DecVal = SeqDecVal [Decimal.Seq] | RangeDecVal Decimal.Seq Decimal.Seq

data HexVal = SeqHexVal [Hex.Seq] | RangeHexVal Hex.Seq Hex.Seq

newtype ProseVal = ProseVal ByteString

