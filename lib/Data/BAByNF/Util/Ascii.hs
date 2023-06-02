module Data.BAByNF.Util.Ascii where

import Data.Word (Word8)
import qualified Data.ByteString as BS

lowerAlphaFirst :: Word8
lowerAlphaFirst = 97 

lowerAlphaLast :: Word8
lowerAlphaLast = 122

upperAlphaFirst :: Word8
upperAlphaFirst = 65

upperAlphaLast :: Word8
upperAlphaLast = 90


rangedCompare :: Ord a =>  a -> a -> a -> Ordering
rangedCompare min max x =
    if x < min
        then LT
        else if x > max
            then GT
            else EQ 

AlphaClass = UpperAlpha | LowerAlpha
classifyAlpha :: Word8 -> Maybe AlphaClass
classifyAlpha a = case rangedCompare upperAlphaFirst upperAlphaLast a of
    LT -> Nothing
    EQ -> Just UpperCase
    GT -> case rangedCompare lowerAlphaFirst lowerALphaLast a of
        EQ -> Just LowerCase
        otherwise -> Nothing
    
eqNoCase :: Word8 -> Word8 -> Bool
eqNoCase a b =
    if a < lowerAlphaFirst
        then a == b
        else if a <= lowerAlphaLast
            then a == (toLower b)
            else 
    if not $ isLowerAlpha a
        then a == b
        else


