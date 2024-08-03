module Data.BAByNF.ABNF.Model 
    ( Rulelist (..)
    , DefinedAs (..)
    , Rule (..)
    , Rulename (..)
    , Elements (..)
    , Alternation (..)
    , Concatenation (..)
    , Repetition (..)
    , Repeat (..)
    , Bound (..)
    , Element (..)
    , Group (..)
    , Option (..)
    , CharVal (..)
    , CaseInsensitiveString (..)
    , CaseSensitiveString (..)
    , QuotedString (..)
    , NumVal (..)
    , BinVal (..)
    , DecVal (..)
    , HexVal (..)
    , ProseVal (..)
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString.Char8

import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Util.Binary qualified as Binary
import Data.BAByNF.Util.Decimal qualified as Decimal
import Data.BAByNF.Util.Hex qualified as Hex

import Data.BAByNF.Core.Ref (Ref)
import Data.BAByNF.Core.Ref qualified as Ref

newtype Rulelist = Rulelist [Rule] deriving (Eq, Show)

data DefinedAs = BasicDefinition | IncrementalAlternative deriving (Eq, Show)

data Rule = Rule Rulename DefinedAs Elements deriving (Eq, Show)

newtype Rulename = Rulename ByteString deriving Eq

instance Show Rulename where
  show (Rulename b) = "Rulename[" ++ ByteString.Char8.unpack b ++ "]" 

instance Ref Rulename where
  eq (Rulename x) (Rulename y) = Ascii.eqNoCaseBS x y
  display (Rulename x) = show x

newtype Elements = Elements Alternation deriving (Eq, Show)
newtype Alternation = Alternation [Concatenation] deriving (Eq, Show)

newtype Concatenation = Concatenation [Repetition] deriving (Eq, Show)

data Repetition = Repetition Repeat Element deriving (Eq, Show)

data Repeat = NoRepeat | FixedRepeat Integer | RangedRepeat Bound Bound deriving (Eq, Show)
data Bound = UnBound | FixedBound Integer deriving (Eq, Show)

data Element = RulenameElement Rulename
             | GroupElement Group
             | OptionElement Option
             | CharValElement CharVal
             | NumValElement NumVal
             | ProseValElement ProseVal
             deriving (Eq, Show)


newtype Group = Group Alternation deriving (Eq, Show)

newtype Option = Option Alternation deriving (Eq, Show)

data CharVal = CaseInsensitiveCharVal CaseInsensitiveString
             | CaseSensitiveCharVal CaseSensitiveString
             deriving (Eq, Show)

newtype CaseInsensitiveString = CaseInsensitiveString QuotedString deriving (Eq, Show)

newtype CaseSensitiveString = CaseSensitiveString QuotedString deriving (Eq, Show)

newtype QuotedString = QuotedString ByteString deriving Eq

instance Show QuotedString where
  show (QuotedString b) = show (ByteString.Char8.unpack b)

data NumVal = BinNumVal BinVal
            | DecNumVal DecVal
            | HexNumVal HexVal
            deriving (Eq, Show)


data BinVal = SeqBinVal [Binary.Seq] | RangeBinVal Binary.Seq Binary.Seq deriving (Eq, Show)

data DecVal = SeqDecVal [Decimal.Seq] | RangeDecVal Decimal.Seq Decimal.Seq deriving (Eq, Show)

data HexVal = SeqHexVal [Hex.Seq] | RangeHexVal Hex.Seq Hex.Seq deriving (Eq, Show)

newtype ProseVal = ProseVal ByteString deriving Eq
instance Show ProseVal where
  show (ProseVal b) = "ProseVal " ++ show (ByteString.Char8.unpack b)
