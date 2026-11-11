module ParserFixtures
    ( bytes, ref, literal, grammar, parseStart, treeWeight
    , httpGrammar, httpRequest, smtpGrammar, smtpResponse
    ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Attoparsec.ByteString qualified as A
import Data.BAByNF.ABNF.Core qualified as Core
import Data.BAByNF.ABNF.Model qualified as M
import Data.BAByNF.ABNF.Parse qualified as ABNF
import Data.BAByNF.Core.Parseable (Parseable (Unit))
import Data.BAByNF.Core.Tree (Tree (Tree), Node (StringNode, RefNode))

bytes :: String -> ByteString
bytes = B8.pack

ref :: String -> M.Rulename
ref = M.Rulename . bytes

literal :: String -> Parseable M.Rulename
literal s = Unit (show s) $ A.string (bytes s) >>= pure . Tree . pure . StringNode

-- Fixtures are intentionally small protocol subsets, not complete RFC grammars.
grammar :: [String] -> M.Rulelist
grammar lines' = case ABNF.parseRulelist (bytes (concatMap (++ "\r\n") lines')) of
    Left err -> error ("invalid test fixture: " ++ err)
    Right (M.Rulelist rs) -> M.Rulelist (rs ++ Core.rules)

parseStart :: M.Rulelist -> ByteString -> Either String (Tree M.Rulename)
parseStart rs = ABNF.parse rs (ref "start")

-- Traverse every output node without adding Show/pretty-print costs to timings.
treeWeight :: Tree M.Rulename -> Int
treeWeight (Tree ns) = sum (map nodeWeight ns)
    where nodeWeight (StringNode b) = BS.length b
          nodeWeight (RefNode (M.Rulename r) t) = BS.length r + treeWeight t

httpGrammar :: M.Rulelist
httpGrammar = grammar
    [ "start = method SP target SP %s\"HTTP/1.1\" CRLF *(header CRLF) CRLF"
    , "method = %s\"GET\" / %s\"POST\""
    , "target = \"/\" *(ALPHA / DIGIT / \"/\" / \"-\")"
    , "header = token \":\" *SP value"
    , "token = 1*(ALPHA / DIGIT / \"-\")"
    , "value = *(%x20-7E)"
    ]

httpRequest :: ByteString
httpRequest = bytes "GET /api/items HTTP/1.1\r\nHost: example.com\r\nUser-Agent: babynf\r\n\r\n"

smtpGrammar :: M.Rulelist
smtpGrammar = grammar
    [ "start = (\"250\" SP Domain [SP greeting] CRLF) / (\"250-\" Domain [SP greeting] CRLF *(\"250-\" line CRLF) \"250\" SP line CRLF)"
    , "Domain = \"mail.server.org\""
    , "greeting = 1*(%d0-9 / %d11-12 / %d14-127)"
    , "line = keyword *(SP param)"
    , "keyword = (ALPHA / DIGIT) *(ALPHA / DIGIT / \"-\")"
    , "param = 1*(%d33-126)"
    ]

smtpResponse :: ByteString
smtpResponse = bytes "250-mail.server.org greetings\r\n250-PIPELINING\r\n250 SIZE 10485760\r\n"
