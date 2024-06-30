module Data.BAByNF.Util.Decimal where

data Digit = D0 
           | D1
           | D2
           | D3
           | D4
           | D5
           | D6
           | D7
           | D8
           | D9 
           deriving (Eq, Ord)
newtype Seq = Seq [Digit] deriving Eq

val :: Integral a => Digit -> a
val D0 = 0
val D1 = 1
val D2 = 2
val D3 = 3
val D4 = 4
val D5 = 5
val D6 = 6
val D7 = 7
val D8 = 8
val D9 = 9

toNum :: Integral a => Seq -> a
toNum (Seq digits) = toNum' digits 0
    where toNum' [] acc = acc
          toNum' (d : ds) acc = 
            let newAcc = (acc * 10) + val d
             in toNum' ds newAcc

fromVal :: Integral a => a -> Maybe Digit
fromVal 0 = Just D0
fromVal 1 = Just D1
fromVal 2 = Just D2
fromVal 3 = Just D3
fromVal 4 = Just D4
fromVal 5 = Just D5
fromVal 6 = Just D6
fromVal 7 = Just D7
fromVal 8 = Just D8
fromVal 9 = Just D9
fromVal _ = Nothing

instance Show Seq where
  show (Seq x) = map toChar x

toChar :: Digit -> Char
toChar d = case d of
  D0 -> '0'
  D1 -> '1'
  D2 -> '2'
  D3 -> '3'
  D4 -> '4'
  D5 -> '5'
  D6 -> '6'
  D7 -> '7'
  D8 -> '8'
  D9 -> '9'
