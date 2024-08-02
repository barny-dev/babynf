module Data.BAByNF.ABNF.Parse
    ( parse
    , parseRulelist
    ) where

import Data.List qualified as List

import Data.ByteString (ByteString)

import Data.Attoparsec.ByteString qualified as Attoparsec.ByteString

import Data.BAByNF.Core.Ref qualified as Ref

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.Core.RefDict (RefDict (..))
import Data.BAByNF.Core.Parseable qualified as Parseable
import Data.BAByNF.Core.Parseable (Parseable)
import Data.BAByNF.ABNF.Model qualified as Model
import Data.BAByNF.ABNF.Rules.Rulelist qualified as Rulelist
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.ToParseable

-- TODO: split parse til end of input?
parse :: ToParseable p => Model.Rulelist -> p -> ByteString -> Either String (Tree Model.Rulename)
parse r p t = 
    let parser = Parseable.toParser (toRefDict r) (toParseable p) 
     in Attoparsec.ByteString.parseOnly parser t

parseRulelist :: ByteString -> Either String Model.Rulelist
parseRulelist t =
    let toTree = parse rules Rulelist.ref t
     in toTree
        >>= \tree -> case Tree.nodes tree of
            [Tree.RefNode ref subtree]
                | Ref.eq ref Rulelist.ref -> Right subtree
                | otherwise -> Left "structural mismatch for <rulelist>"
            _ -> Left "structural mismatch for <rulelist>"
        >>= joinErrors . Rulelist.fromTree
     where joinErrors (Right x) = Right x
           joinErrors (Left errors) = Left $ "Errors found:\n" ++ List.intercalate "\n" errors

toRefDict :: Model.Rulelist -> RefDict Model.Rulename (Parseable Model.Rulename)
toRefDict (Model.Rulelist r) = RefDict (map (\(Model.Rule ref _ (Model.Elements a)) -> (ref, toParseable a)) r)
