module Data.BAByNF.Util.Ascii
    ( lowerAlphaFirst
    , lowerAlphaLast
    , upperAlphaFirst
    , upperAlphaLast
    , rangedCompare
    , AlphaClass (..)
    , classifyAlpha
    , lowerToUpperUnsafe
    , lowerToUpper
    , eqNoCase
    , eqNoCaseSeq
    , eqNoCaseBS
    , fromChar
    , fromCharOrNull
    , bs
    , parseHex
    , toHexDigit
    , bsToHexDigit
    , toHexSeq
    , toDecimalDigit
    , bsToDecimalDigit
    , toBinaryDigit
    , bsToBinaryDigit
    , stringAsBytesUnsafe
    , parseCaseInsensitive
    , parseCaseSensitive
    ) where

import Data.Functor ((<&>))
import Data.Char qualified as Char
import Data.Maybe qualified as Maybe
import Data.Word (Word8)
import Data.List qualified as List

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8

import Data.Attoparsec.ByteString qualified as Attoparsec.ByteString

import Data.BAByNF.Util.Binary qualified as Binary
import Data.BAByNF.Util.Decimal qualified as Decimal
import Data.BAByNF.Util.Hex qualified as Hex
import Control.Applicative ((<|>))

lowerAlphaFirst :: Word8
lowerAlphaFirst = 97

lowerAlphaLast :: Word8
lowerAlphaLast = 122

upperAlphaFirst :: Word8
upperAlphaFirst = 65

upperAlphaLast :: Word8
upperAlphaLast = 90


rangedCompare :: Ord a =>  a -> a -> a -> Ordering
rangedCompare lo hi x
  | x < lo = LT
  | x > hi = GT
  | otherwise = EQ

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
                   then a == lowerToUpperUnsafe b
                   else b == lowerToUpperUnsafe a

eqNoCaseSeq :: [Word8] -> [Word8] -> Bool
eqNoCaseSeq [] [] = True
eqNoCaseSeq _ [] = False
eqNoCaseSeq [] _ = False
eqNoCaseSeq (x:xs) (y:ys) = eqNoCase x y && eqNoCaseSeq xs ys

eqNoCaseBS :: ByteString -> ByteString -> Bool
eqNoCaseBS a b = ByteString.length a == ByteString.length b && all eq' pairs
    where eq' = uncurry eqNoCase
          pairs = ByteString.zip a b

fromChar :: Char -> Maybe Word8
fromChar ch =
    if Char.isAscii ch
        then Just (fromIntegral (Char.ord ch))
        else Nothing

fromCharOrNull :: Char -> Word8
fromCharOrNull ch = Maybe.fromMaybe 0 (fromChar ch)

bs :: Char -> ByteString
bs ch = ByteString.singleton (fromCharOrNull ch)

parseHex :: (Integral a) => ByteString -> Maybe a
parseHex s = fmap (Hex.toNum . Hex.Seq) (mapM toHexDigit (ByteString.unpack s))

toHexDigit :: Word8 -> Maybe Hex.Digit
toHexDigit w
    | w >= 48 && w <= 57 = Hex.fromVal (w - 48)
    | w >= 97 && w <= 102 = Hex.fromVal (w - 97 + 10)
    |  w >= 65 && w <= 70 = Hex.fromVal (w - 65 + 10)
    | otherwise = Nothing

bsToHexDigit :: ByteString -> Maybe Hex.Digit
bsToHexDigit b =
    case ByteString.uncons b of
        Just (w, t) | ByteString.null t -> toHexDigit w
                    | otherwise -> Nothing
        _ -> Nothing
toHexSeq :: ByteString -> Maybe Hex.Seq
toHexSeq b = toHexDigs b <&> Hex.Seq
    where toHexDigs x = ByteString.uncons x >>=
            \(h, rest) ->  toHexDigit h >>=
            \hexdig -> if ByteString.null rest
                then Just [hexdig]
                else toHexDigs rest >>= \hexdigs -> Just (hexdig:hexdigs)

toDecimalDigit :: Word8 -> Maybe Decimal.Digit
toDecimalDigit w
    | w >= 48 && w <= 57 = Decimal.fromVal (w - 48)
    | otherwise = Nothing

bsToDecimalDigit :: ByteString -> Maybe Decimal.Digit
bsToDecimalDigit b =
    case ByteString.uncons b of
        Just (w, t) | ByteString.null t -> toDecimalDigit w
                    | otherwise -> Nothing
        _ -> Nothing

toBinaryDigit :: Word8 -> Maybe Binary.Digit
toBinaryDigit w
    | w `elem` [48, 49] = Binary.fromVal (w - 48)
    | otherwise = Nothing

bsToBinaryDigit :: ByteString -> Maybe Binary.Digit
bsToBinaryDigit b =
    case ByteString.uncons b of
        Just (w, t) | ByteString.null t -> toBinaryDigit w
                    | otherwise -> Nothing
        _ -> Nothing

stringAsBytesUnsafe :: String -> ByteString
stringAsBytesUnsafe s =
    case List.find (not . Char.isAscii) s of
        Just _ -> error "string contains non-ascii characters"
        Nothing -> ByteString.Char8.pack s

parseCaseInsensitive :: ByteString -> Attoparsec.ByteString.Parser ByteString
parseCaseInsensitive b = Attoparsec.ByteString.take (ByteString.length b)
    >>= \b' -> if b `eqNoCaseBS` b' then return b' else fail "case insensitive match fail"

parseCaseSensitive :: ByteString -> Attoparsec.ByteString.Parser ByteString
parseCaseSensitive b = Attoparsec.ByteString.string b <|> fail "case sensitive match fail"
