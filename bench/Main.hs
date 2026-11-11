module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Attoparsec.ByteString qualified as A
import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, nf)
import Data.BAByNF.ABNF.Model qualified as M
import Data.BAByNF.ABNF.ToParseable (toParseable)
import Data.BAByNF.Core.Parseable (Parseable (..), toParser)
import Data.BAByNF.Core.RefDict (RefDict (RefDict))
import Data.BAByNF.Core.Repeat qualified as R
import Data.BAByNF.Core.Tree qualified as Tree
import ParserFixtures

main :: IO ()
main = do
    -- Force fixtures before measurement and reject broken benchmark scenarios.
    mapM_ (\(name, parser, input, accepted) ->
        case A.parseOnly parser input of
            Right tree | accepted -> treeWeight tree `seq` pure ()
            Left _ | not accepted -> pure ()
            result -> error ("invalid benchmark fixture " ++ name ++ ": " ++ show result)) cases
    defaultMain
        [ bgroup "protocol" [scenario "HTTP request" httpParser httpRequest True
                            ,scenario "SMTP multiline" smtpParser smtpResponse True]
        , bgroup "flat repetition" [scenario (show n) repeated (BS.replicate n 97) True | n <- [64, 256, 1024]]
        , bgroup "overlapping alternatives"
            [scenario (show n) (overlapping n) (bytes (replicate n 'a' ++ "b")) True | n <- [4, 16, 32]]
        , bgroup "right recursion" [scenario (show n) recursive (BS.replicate n 97) True | n <- [16, 64, 128]]
        , bgroup "late rejection" [scenario (show n) repeated (bytes (replicate n 'a' ++ "!")) False | n <- [64, 256]]
        ]
    where cases = [("HTTP", httpParser, httpRequest, True), ("SMTP", smtpParser, smtpResponse, True)
                  ,("repetition", repeated, bytes "aaa", True), ("alternatives", overlapping 4, bytes "aaaab", True)
                  ,("recursion", recursive, bytes "aaa", True), ("rejection", repeated, bytes "aaa!", False)]

httpParser, smtpParser :: A.Parser DataTree
httpParser = protocolParser httpGrammar
smtpParser = protocolParser smtpGrammar

type DataTree = Tree.Tree M.Rulename

protocolParser :: M.Rulelist -> A.Parser DataTree
protocolParser (M.Rulelist rs) = toParser
    (RefDict [(r, toParseable a) | M.Rule r _ (M.Elements a) <- rs]) (Rule (ref "start"))

repeated, recursive :: A.Parser DataTree
repeated = toParser (RefDict []) (Rep (literal "a") R.zeroOrMore)
recursive = toParser (RefDict [(ref "r", Alt (Seq (literal "a" :| [Rule (ref "r")]) :| [literal ""]))]) (Rule (ref "r"))

overlapping :: Int -> A.Parser DataTree
overlapping n = toParser (RefDict []) $ Seq
    (Alt (literal "a" :| [literal (replicate k 'a') | k <- [2 .. n]]) :| [literal "b"])

scenario :: String -> A.Parser DataTree -> ByteString -> Bool -> Benchmark
scenario name parser input accepted = bench name $ nf score input
    where score bs = case A.parseOnly parser bs of
            Right tree | accepted -> treeWeight tree
            Left err | not accepted -> length err
            _ -> error "benchmark outcome changed"
