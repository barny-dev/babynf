module Data.BAByNF.KnownIssues.Test (testModule, runProbe) where

import Control.Exception (evaluate)
import Data.ByteString qualified as BS
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (ExitSuccess), die)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Data.BAByNF.ABNF.Parse qualified as ABNF
import Data.BAByNF.Core.Tree qualified as T
import ParserFixtures (bytes, ref)

-- These are desired outcomes, not assertions that defects should be preserved.
-- Unexpected passes fail CI, prompting removal of the expected-failure marker.
testModule :: TestTree
testModule = testGroup "known issues (expected failures)"
    [ testCase "probe harness accepts valid input" $ do
        result <- probe "control-valid"
        fmap (\(code, out, _) -> (code, words out)) result @?= Just (ExitSuccess, ["accepted"])
    , testCase "probe harness rejects invalid input normally" $ do
        result <- probe "control-invalid"
        fmap (\(code, out, _) -> (code, words out)) result @?= Just (ExitSuccess, ["rejected"])
    , issue "duplicate-basic" "duplicate basic definitions must be rejected"
    , issue "orphan-incremental" "incremental definitions need a basic definition"
    , issue "undefined-alternative" "undefined references must be diagnosed even in an unselected alternative"
    , issue "decimal-overflow" "decimal terminals must not wrap to a byte"
    , issue "hex-overflow" "hex terminals must not wrap to a byte"
    , issue "binary-overflow" "binary terminals must not wrap to a byte"
    , issue "reversed-repeat" "reversed repetition bounds must return a failure rather than throw"
    , issue "nullable-repeat" "unbounded nullable repetition must terminate with a failure"
    , issue "direct-left-recursion" "unsupported direct left recursion must terminate with a failure"
    , issue "indirect-left-recursion" "unsupported indirect left recursion must terminate with a failure"
    ]
    where issue name reason = expectFailBecause reason $ testCase name $ do
            result <- probe name
            case result of
                Just (ExitSuccess, out, _) | words out == ["rejected"] -> pure ()
                Just (code, out, err) -> assertFailure
                    ("expected a normal parser rejection; got " ++ show code ++ ": " ++ out ++ err)
                Nothing -> assertFailure "parser did not terminate within 5 seconds"

probe :: String -> IO (Maybe (ExitCode, String, String))
probe name = do
    exe <- getExecutablePath
    timeout (5 * 1000000) $ readProcessWithExitCode exe
        ["--babynf-probe", name, "+RTS", "-M64m", "-K8m", "-RTS"] ""

-- Spawned through the test executable so this also works from an sdist and on
-- Windows. The parent kills timed-out processes; RTS limits contain divergence.
runProbe :: String -> IO ()
runProbe name = case lookup name probes of
    Nothing -> die ("unknown probe: " ++ name)
    Just (source, input) -> do
        let result = ABNF.parseRulelist (bytes source) >>= \rs -> ABNF.parse rs (ref "start") input
        case result of
            Left _ -> putStrLn "rejected"
            Right tree -> do
                _ <- evaluate (BS.length (T.stringify tree))
                putStrLn "accepted"
    where probes =
            [ ("control-valid", ("start = \"a\"\r\n", bytes "a"))
            , ("control-invalid", ("start = \"a\"\r\n", bytes "b"))
            , ("duplicate-basic", ("start = \"a\"\r\nstart = \"b\"\r\n", bytes "a"))
            , ("orphan-incremental", ("start =/ \"a\"\r\n", bytes "a"))
            , ("undefined-alternative", ("start = missing / \"a\"\r\n", bytes "a"))
            , ("decimal-overflow", ("start = %d256\r\n", BS.singleton 0))
            , ("hex-overflow", ("start = %x100\r\n", BS.singleton 0))
            , ("binary-overflow", ("start = %b100000000\r\n", BS.singleton 0))
            , ("reversed-repeat", ("start = 3*2\"a\"\r\n", bytes "aaa"))
            , ("nullable-repeat", ("start = *[\"a\"]\r\n", bytes ""))
            , ("direct-left-recursion", ("start = start / \"a\"\r\n", bytes "a"))
            , ("indirect-left-recursion", ("start = other / \"a\"\r\nother = start\r\n", bytes "a"))
            ]
