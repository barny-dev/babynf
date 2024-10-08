module Data.BAByNF.Util.Binary
    ( Digit (..)
    , Seq (..)
    , val
    , toNum
    , fromVal
    , toChar
    ) where

data Digit = B0 | B1 deriving (Eq, Ord)
newtype Seq = Seq [Digit] deriving Eq

val :: Integral a => Digit -> a
val B0 = 0
val B1 = 1

toNum :: Integral a => Seq -> a
toNum (Seq digits) = toNum' digits 0
    where toNum' [] acc = acc
          toNum' (d : ds) acc =
            let newAcc = (acc * 2) + val d
             in toNum' ds newAcc

fromVal :: Integral a => a -> Maybe Digit
fromVal 0 = Just B0
fromVal 1 = Just B1
fromVal _ = Nothing

instance Show Seq where
  show (Seq x) = map toChar x

toChar :: Digit -> Char
toChar B0 = '0'
toChar B1 = '1'