module Data.BAByNF.Util.Ascii.Test where

import Data.Word (Word8)
import Data.Char (chr, ord)

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as TH

import qualified Data.ByteString as BS

import qualified Data.BAByNF.Util.Ascii as A

moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.Util.Ascii"

testModule :: T.TestTree
testModule = moduleUnderTest `T.testGroup`
    [ rangedCompareTest
    , lowerToUpperTest
    , eqNoCaseTest
    , eqNoCaseSeqTest
    , eqNoCaseBSTest
    ]

rangedCompareTest :: T.TestTree
rangedCompareTest = "rangedCompare test" `T.testGroup`
    [ "rangedCompare given value below lower bound" `TH.testCase`
        ( TH.assertEqual "should be less than" LT (funcUnderTest 1) )
    , "rangedCompare given value equal to lower bound" `TH.testCase`
        ( TH.assertEqual "should be equal" EQ (funcUnderTest 2) )
    , "rangedCompare given value between lower and upper bound" `TH.testCase`
        ( TH.assertEqual "should be equal" EQ (funcUnderTest 3) )
    , "rangedCompare given value equal to upper bound" `TH.testCase`
        ( TH.assertEqual "should be equal" EQ (funcUnderTest 4) )
    , "rangedCompare given value above bound" `TH.testCase`
        ( TH.assertEqual "should be greater than" GT (funcUnderTest 5) )
    ]
    where funcUnderTest = A.rangedCompare 2 4 :: Word8 -> Ordering

lowerToUpperTest :: T.TestTree
lowerToUpperTest = "lowerToUpper test" `TH.testCase` (
    mapM_ (uncurry doAssert) $
        [ (input, Just expected) | (input, expected) <- zip ['a'..'z'] ['A'..'Z'] ] ++
        [ (input, Nothing) | input <- ['\0'..'`'] ++ ['{'..'~'] ]
    )
    where funcUnderTest input= fromW8 <$> (A.lowerToUpper.toW8) input
          msg input expected = "lowerToUpper given " ++ (show input) ++ " should return " ++ (show expected)
          doAssert input expected = TH.assertEqual (msg input expected) expected (funcUnderTest input)

eqNoCaseTest :: T.TestTree
eqNoCaseTest = "eqNoCase test" `TH.testCase` (
    mapM_ (\(a, b, expected) -> doAssert a b expected) $
        [ (a, b, True) | (a, b) <- zip lc uc ++ zip lc lc ++ zip uc lc ++ zip uc uc ] ++
        [ (a, b, False) | a <- ['A'..'Z'] ++ ['a'..'z'], b <- ['\0'..'@'] ++ ['['..'`'] ++ ['{'..'~'] ]
    )
    where funcUnderTest a b = A.eqNoCase (toW8 a) (toW8 b)
          msg a b expected = "eqNoCase given " ++ show a ++ " and " ++ show b ++ " should return " ++ show expected
          lc = ['a'..'z']
          uc = ['A'..'Z']
          doAssert a b expected = TH.assertEqual (msg a b expected) expected (funcUnderTest a b) 

eqNoCaseSeqTest :: T.TestTree
eqNoCaseSeqTest = "eqNoCaseSeq tests" `T.testGroup`
    [ "eqNoCaseSeq test - empty" `TH.testCase` ( doAssert "" "" True )
    , "eqNoCaseSeq test - one empty" `TH.testCase` ( doAssert "" "ab;" False)
    , "eqNoCaseSeq test - exact same" `TH.testCase` ( doAssert "a! nGr\n." "a! nGr\n." True)
    , "eqNoCaseSeq test - case mirror" `TH.testCase` ( doAssert "A! NgR\n." "a! nGr\n." True)
    , "eqNoCaseSeq test - different" `TH.testCase` ( doAssert "A1\rnG\\." "Pangolin! Webbed feet..." False)
    ]
    where funcUnderTest a b = A.eqNoCaseSeq (map toW8 a)  (map toW8 b)
          msg a b expected = "eqNoCaseSeq given " ++ show a ++ " and " ++ show b ++ " should return " ++ show expected
          doAssert a b expected = TH.assertEqual (msg a b expected) expected (funcUnderTest a b)

eqNoCaseBSTest :: T.TestTree
eqNoCaseBSTest = "eqNoCaseBS test" `T.testGroup`
    [ "eqNoCaseBS test - empty" `TH.testCase` ( doAssert "" "" True )
    , "eqNoCaseBS test - one empty" `TH.testCase` ( doAssert "" "ab;" False)
    , "eqNoCaseBS test - exact same" `TH.testCase` ( doAssert "a! nGr\n." "a! nGr\n." True)
    , "eqNoCaseBS test - case mirror" `TH.testCase` ( doAssert "A! NgR\n." "a! nGr\n." True)
    , "eqNoCaseBS test - different" `TH.testCase` ( doAssert "A1\rnG\\." "Pangolin! Webbed feet..." False)
    ]
    where funcUnderTest a b = A.eqNoCaseBS (BS.pack $ map toW8 a)  (BS.pack $ map toW8 b)
          msg a b expected = "eqNoCaseBS given " ++ show a ++ " and " ++ show b ++ " should return " ++ show expected
          doAssert a b expected = TH.assertEqual (msg a b expected) expected (funcUnderTest a b)

toW8 :: Char -> Word8
toW8 = fromIntegral.ord

fromW8 :: Word8 -> Char
fromW8 = chr.fromIntegral

