module Data.BAByNF.Core.Ref (Ref, ByteStringRef, CaseInsensitiveAsciiRef, bytestring, caseInsensitiveAscii) where

import Data.ByteString (ByteString)
import qualified Data.BAByNF.Util.Ascii as Ascii

class (Eq a, Show a) => Ref a

newtype ByteStringRef = ByteStringRef ByteString deriving (Eq, Show)
instance Ref ByteStringRef

bytestring :: ByteString -> ByteStringRef
bytestring = ByteStringRef

newtype CaseInsensitiveAsciiRef = CaseInsensitiveAsciiRef ByteString deriving Show
instance Eq CaseInsensitiveAsciiRef where
    (==) (CaseInsensitiveAsciiRef a) (CaseInsensitiveAsciiRef b) = Ascii.eqNoCaseBS a b
     
instance Ref CaseInsensitiveAsciiRef

caseInsensitiveAscii :: ByteString -> CaseInsensitiveAsciiRef
caseInsensitiveAscii = CaseInsensitiveAsciiRef

