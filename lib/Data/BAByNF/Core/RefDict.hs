module Data.BAByNF.Core.RefDict
    ( RefDict (..)
    , lookup
    , lookup1
    ) where

import Prelude hiding (lookup)

import Data.BAByNF.Core.Ref (Ref)
import Data.BAByNF.Core.Ref qualified as Ref

data RefDict a b where RefDict :: (Ref a) => [(a, b)] -> RefDict a b
deriving instance (Show a, Show b) => Show (RefDict a b)

lookup :: a -> RefDict a b -> [b]
lookup ref (RefDict list) = lookup' ref list
    where lookup' r l = case l of
            [] -> []
            (r', e) : rest ->
                let cont = lookup' r rest
                    in if Ref.eq r r' 
                    then e : cont
                    else cont

lookup1 :: a -> RefDict a b -> Maybe b
lookup1 ref (RefDict list) = lookup1' ref list
    where lookup1' r l = case l of
            [] -> Nothing
            (r', e) : rest ->
                if Ref.eq r r'
                    then Just e
                    else lookup1' ref rest
                
