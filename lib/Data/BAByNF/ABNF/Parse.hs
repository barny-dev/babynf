module Data.BAByNF.ABNF.Parse where

import Data.ByteString (ByteString)

import Data.Attoparsec.ByteString qualified as Attoparsec.ByteString

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Parseable qualified as Parseable

import Data.BAByNF.ABNF

-- TODO: split parse til end of input?
parse :: ToParseable p => Rulelist -> p -> ByteString -> Either String (Tree Rulename)
parse r p t = 
    let parser = Parseable.toParser (toRefDict r) (toParseable p) 
     in Attoparsec.ByteString.parseOnly parser t
