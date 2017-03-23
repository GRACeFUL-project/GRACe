{-# LANGUAGE ScopedTypeVariables #-}
module Main where


import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import System.Exit (ExitCode)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit
import System.Process

import TestOutParser (tests)
import TestFW.TestGCMCheck (tests)

main :: IO ()
main = do
    -- Setup
    server <- spawnCommand "stack exec RestAPI"
    threadDelay 1000000  -- wait a second for the server to get started

    -- Test
    defaultMain (testGroup "Test all"
      [ TestOutParser.tests
      , TestFW.TestGCMCheck.tests
      , serviceTests
      ]) `catch` \(e :: ExitCode) -> return ()

    -- Teardown
    terminateProcess server

serviceTests = testGroup "Unit tests" 
  [ testService "library/crud" [] "test/test_library.exp"
  , testService "submit" [ "-H", "Content-Type: application/json"
                         , "--data", "@example.json"] "test/test_submit.exp"
  ]

testService :: String -> [String] -> FilePath -> TestTree
testService endpoint options file = testCase ("Testing: " ++ file) $ do
    out <- readProcess "curl" (options ++ ["-s", url]) "" 
    exp <- readFile file
    out @?= exp
  where
    url = "http://localhost:8081/" ++ endpoint 
