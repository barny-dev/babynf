module Main (main) where

import qualified Test.Tasty as T

import qualified Data.BAByNF.Test as BT

main :: IO ()
main = T.defaultMain BT.tests

