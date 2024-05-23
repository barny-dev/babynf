{-# LANGUAGE LambdaCase #-}
module Data.BAByNF.ABNF.Rules.CharVal
    ( ref
    , rule
    , fromTree
    ) where

import Data.Functor ((<&>))
import Data.Word (Word8)

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF.Grammar ((+!), (|!), (??), (+?))
import Data.BAByNF.ABNF.Grammar qualified as Grammar
import Data.BAByNF.ABNF.Core qualified as Core

import Data.Maybe (listToMaybe, fromMaybe)
import Data.Functor ((<&>))
import Data.List qualified as List

import Control.Applicative ((<|>))

import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Util.Binary qualified as Binary
import Data.BAByNF.Util.Stream (Stream)
import Data.BAByNF.Util.Stream qualified as Stream

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Rules.CaseInsensitiveString qualified as CaseInsensitiveString
import Data.BAByNF.ABNF.Rules.CaseSensitiveString qualified as CaseSensitiveString
import Data.BAByNF.ABNF.Core qualified as Core

ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "char-val")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition
    . ABNF.Elements
    . ABNF.Alternation
    $
        [ ABNF.Concatenation . List.singleton . ABNF.Repetition ABNF.NoRepeat $ ABNF.RulenameElement CaseInsensitiveString.ref
        , ABNF.Concatenation . List.singleton . ABNF.Repetition ABNF.NoRepeat $ ABNF.RulenameElement CaseSensitiveString.ref
        ]

fromTree :: Tree ABNF.Rulename -> Either String ABNF.CharVal -- TODO: support RFC 7405
fromTree tree =
    fromMaybe (Left "no string") $ tryGetInsensitive tree <|> tryGetSensitive tree
    where tryGetInsensitive t = do
            subtree <- Tree.getChildWithRef CaseInsensitiveString.ref t
            return $ CaseInsensitiveString.fromTree subtree <&> ABNF.CaseInsensitiveCharVal
          tryGetSensitive t = do
            subtree <- Tree.getChildWithRef CaseSensitiveString.ref t
            return $ CaseSensitiveString.fromTree subtree <&> ABNF.CaseSensitiveCharVal 



    -- maybe (Left "char-val must be between \" and \"") Right $
    --     unconsnoc (Tree.stringify tree) >>= \(h, m, l) ->
    --         if h == 34 && l == 34
    --             then Just (Grammar.ArrayTerm Grammar.CaseInsensitive m)
    --             else Nothing

-- unconsnoc :: ByteString -> Maybe (Word8, ByteString, Word8)
-- unconsnoc bs = ByteString.uncons bs >>= \(h, t) -> ByteString.unsnoc t <&> \(m, l) -> (h, m, l)

