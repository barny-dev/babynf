module Data.BAByNF.Util.Hex where

data Digit
    = X0
    | X1
    | X2
    | X3
    | X4
    | X5
    | X6
    | X7
    | X8
    | X9
    | XA
    | XB
    | XC
    | XD
    | XE
    | XF
newtype Seq = Seq [Digit]

val :: (Integral a) => Digit -> a
val X0 = 0
val X1 = 1
val X2 = 2
val X3 = 3
val X4 = 4
val X5 = 5
val X6 = 6
val X7 = 7
val X8 = 8
val X9 = 9
val XA = 10
val XB = 11
val XC = 12
val XD = 13
val XE = 14
val XF = 15

toNum :: (Integral a) => Seq -> a
toNum (Seq digits) = toNum' digits 0
    where toNum' [] acc = acc
          toNum' (d : ds) acc = 
            let newAcc = (acc * 16) + val d
             in toNum' ds newAcc

fromVal :: (Integral a) => a -> Maybe Digit
fromVal 0 = Just X0
fromVal 1 = Just X1
fromVal 2 = Just X2
fromVal 3 = Just X3
fromVal 4 = Just X4
fromVal 5 = Just X5
fromVal 6 = Just X6
fromVal 7 = Just X7
fromVal 8 = Just X8
fromVal 9 = Just X9
fromVal 10 = Just XA
fromVal 11 = Just XB
fromVal 12 = Just XC
fromVal 13 = Just XD
fromVal 14 = Just XE
fromVal 15 = Just XF
fromVal _ = Nothing