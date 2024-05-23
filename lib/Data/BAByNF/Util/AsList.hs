{-# LANGUAGE TypeFamilies #-}
module Data.BAByNF.Util.AsList where

class AsList l where
    type Element l
    asList :: l -> [Element l]