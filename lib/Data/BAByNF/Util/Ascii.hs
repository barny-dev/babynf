module Data.BAByNF.Util.Ascii where

import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8 ()

lowerAlphaFirst :: Word8
lowerAlphaFirst = 97 

lowerAlphaLast :: Word8
lowerAlphaLast = 122

upperAlphaFirst :: Word8
upperAlphaFirst = 65

upperAlphaLast :: Word8
upperAlphaLast = 90


rangedCompare :: Ord a =>  a -> a -> a -> Ordering
rangedCompare lo hi x =
    if x < lo
        then LT
        else if x > hi
            then GT
            else EQ 

data AlphaClass = UpperAlpha | LowerAlpha deriving (Eq, Show)

classifyAlpha :: Word8 -> Maybe AlphaClass
classifyAlpha a = case rangedCompare upperAlphaFirst upperAlphaLast a of
    LT -> Nothing
    EQ -> Just UpperAlpha
    GT -> case rangedCompare lowerAlphaFirst lowerAlphaLast a of
        EQ -> Just LowerAlpha
        _ -> Nothing

lowerToUpperUnsafe :: Word8 -> Word8
lowerToUpperUnsafe a = a - lowerUpperDiff
    where lowerUpperDiff = lowerAlphaFirst - upperAlphaFirst

lowerToUpper :: Word8 -> Maybe Word8
lowerToUpper a = do
    alphaClass <- classifyAlpha a
    if alphaClass == LowerAlpha
        then Just (lowerToUpperUnsafe a)
        else Nothing

eqNoCase :: Word8 -> Word8 -> Bool
eqNoCase a b =
    case classifyAlpha a of
        Nothing -> a == b
        Just ac -> case classifyAlpha b of
           Nothing -> False
           Just bc -> if bc == ac
               then a == b
               else if ac == UpperAlpha
                   then a == (lowerToUpperUnsafe b)
                   else b == (lowerToUpperUnsafe a)

eqNoCaseSeq :: [Word8] -> [Word8] -> Bool
eqNoCaseSeq [] [] = True
eqNoCaseSeq _ [] = False
eqNoCaseSeq [] _ = False
eqNoCaseSeq (x:xs) (y:ys) = eqNoCase x y && eqNoCaseSeq xs ys

eqNoCaseBS :: BS.ByteString -> BS.ByteString -> Bool
eqNoCaseBS a b = (BS.length a) == (BS.length b) && all eq' pairs
    where eq' = uncurry eqNoCase
          pairs = BS.zip a b

fromChar :: Char -> BS.ByteString