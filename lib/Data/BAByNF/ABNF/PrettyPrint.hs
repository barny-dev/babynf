module Data.BAByNF.ABNF.PrettyPrint
    ( PrettyPrint
    , prettyPrint
    ) where

import Data.List qualified as List

import Data.ByteString.Char8 qualified as ByteString.Char8

import Data.BAByNF.ABNF.Model qualified as Model

class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint Model.Rulelist where
  prettyPrint (Model.Rulelist x) = concatMap ((++ "\r\n") . prettyPrint) x

instance PrettyPrint Model.Rule where
  prettyPrint (Model.Rule rulename definedAs elements) = prettyPrint rulename ++ " " ++ prettyPrint definedAs ++ " " ++ prettyPrint elements

instance PrettyPrint Model.Rulename where
  prettyPrint (Model.Rulename x) = ByteString.Char8.unpack x

instance PrettyPrint Model.DefinedAs where
  prettyPrint Model.BasicDefinition = "="
  prettyPrint Model.IncrementalAlternative = "=/"

instance PrettyPrint Model.Elements where
  prettyPrint (Model.Elements a) = prettyPrint a

instance PrettyPrint Model.Alternation where
  prettyPrint (Model.Alternation x) = List.intercalate " / " (map prettyPrint x)

instance PrettyPrint Model.Concatenation where
  prettyPrint (Model.Concatenation x) = unwords (map prettyPrint x)

instance PrettyPrint Model.Repetition where
  prettyPrint (Model.Repetition r e) = prettyPrint r ++ prettyPrint e

instance PrettyPrint Model.Repeat where
  prettyPrint Model.NoRepeat = ""
  prettyPrint (Model.FixedRepeat i) = show i
  prettyPrint (Model.RangedRepeat mn mx) = toString mn ++ "*" ++ toString mx
    where toString Model.UnBound = ""
          toString (Model.FixedBound i) = show i

instance PrettyPrint Model.Element where
  prettyPrint (Model.RulenameElement x) = prettyPrint x
  prettyPrint (Model.GroupElement x) = prettyPrint x
  prettyPrint (Model.OptionElement x) = prettyPrint x
  prettyPrint (Model.NumValElement x) = prettyPrint x
  prettyPrint (Model.CharValElement x) = prettyPrint x
  prettyPrint (Model.ProseValElement x) = prettyPrint x

instance PrettyPrint Model.Group where
  prettyPrint (Model.Group x) = "(" ++ prettyPrint x ++ ")"

instance PrettyPrint Model.Option where
  prettyPrint (Model.Option x) = "[" ++ prettyPrint x ++ "]"

instance PrettyPrint Model.NumVal where
  prettyPrint x = "%" ++
    ( case x of
      Model.BinNumVal y -> prettyPrint y
      Model.DecNumVal y -> prettyPrint y
      Model.HexNumVal y -> prettyPrint y
    )

instance PrettyPrint Model.BinVal where
  prettyPrint x = "b" ++
    (case x of
      Model.SeqBinVal y -> List.intercalate "." (map show y)
      Model.RangeBinVal y z -> show y ++ "-" ++ show z
    )

instance PrettyPrint Model.DecVal where
  prettyPrint x = "d" ++
    (case x of
      Model.SeqDecVal y -> List.intercalate "." (map show y)
      Model.RangeDecVal y z -> show y ++ "-" ++ show z
    )

instance PrettyPrint Model.HexVal where
  prettyPrint x = "x" ++
    (case x of
      Model.SeqHexVal y -> List.intercalate "." (map show y)
      Model.RangeHexVal y z -> show y ++ "-" ++ show z
    )

instance PrettyPrint Model.CharVal where
  prettyPrint (Model.CaseInsensitiveCharVal x) = prettyPrint x
  prettyPrint (Model.CaseSensitiveCharVal x) = prettyPrint x

instance PrettyPrint Model.CaseInsensitiveString where
  prettyPrint (Model.CaseInsensitiveString x) = prettyPrint x

instance PrettyPrint Model.CaseSensitiveString where
  prettyPrint (Model.CaseSensitiveString x) = "%s" ++ prettyPrint x

instance PrettyPrint Model.QuotedString where
  prettyPrint (Model.QuotedString b) = "\"" ++ ByteString.Char8.unpack b ++ "\""

instance PrettyPrint Model.ProseVal where
  prettyPrint (Model.ProseVal b) = "<" ++ ByteString.Char8.unpack b ++ ">"
