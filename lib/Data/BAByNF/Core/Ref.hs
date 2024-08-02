module Data.BAByNF.Core.Ref
    ( Ref (..)
    ) where

class Ref a where
    eq :: a -> a -> Bool
    display :: a -> String

