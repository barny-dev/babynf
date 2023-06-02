{-# LANGUAGE FunctionalDependencies #-}
module Data.BAByNF.Grammar (someFunc) where

import Data.List.NonEmpty
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString as AP

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data RuleDef = RuleDef Alt
data Elem = RuleRefE RuleRef | GroupE Group | OptE Opt | TermE Term

data RuleRef = RuleRef BS.ByteString

data Group = Group Alt

data Alt = Alt (NonEmpty Concat)

data Concat = Concat (NonEmpty Rep)

data Rep = Rep (Maybe RepDef) Elem
data RepDef = RepDef (Maybe Integer) (Maybe Integer)

data Opt = Opt Alt

data CaseMode = CaseInsensitive | CaseSensitive
data Term = Term CaseMode BS.ByteString

data PRuleRef = PRuleRef RuleRef RuleDef PAlt
instance ToPNode PRuleRef where
    toPNode = PRuleRefN 

data PAlt = PAlt [Concat] (Concat, PConcat) [Concat]
instance ToPNode PAlt where
    toPNode = PAltN

data PConcat = PConcat Concat [PRep]
instance ToPNode PConcat where
    toPNode = PConcatN

data PRep = PRep Rep [PElem]
instance ToPNode PRep where
    toPNode = PRepN

data PElem = PRuleRefE PRuleRef
                | PGroupE PGroup
                | POptE POpt
                | PTermE PTerm
instance ToPNode PElem where
    toPNode = PElemN

data PGroup = PGroup Group PAlt
instance ToPNode PGroup where
    toPNode = PGroupN

data POpt = POpt Opt (Maybe PAlt)
instance ToPNode POpt where
    toPNode = POptN

data PTerm = PTerm Term BS.ByteString
instance ToPNode PTerm where
    toPNode = PTermN

data PNode = PRuleRefN PRuleRef
           | PAltN PAlt
           | PConcatN PConcat
           | PRepN PRep
           | PElemN PElem
           | PGroupN PGroup
           | POptN POpt
           | PTermN PTerm

class ToPNode a where
   toPNode :: a -> PNode

class (ToPNode b) => ToParser a b | a -> b where
    toParser :: a -> AP.Parser b
