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
    server <- spawnProcess "stack" ["exec", "GRACeServer"]
    threadDelay 3000000  -- wait a second for the server to get started

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
  , testService "submit/crud"  [ "-H", "Content-Type: application/json"
                               ,  "--data", "@submit_crud.json"] "test/test_submit.exp"
  , testService "submit/cld"   [ "-H", "Content-Type: application/json"
                               ,  "--data", "@submit_cld_small.json"] "test/test_submit_cld.exp"
  ]

testSubmit :: String -> String -> FilePath -> TestTree
testSubmit lib input exp = testService ("submit" ++ lib) flags exp
 where
  flags = ["-H", "Content-Type: application/json",  "--data", input]

testService :: String -> [String] -> FilePath -> TestTree
testService endpoint options file = testCase ("Testing: " ++ file) $ do
    out <- readProcess "curl" (options ++ ["-s", url]) ""
    exp <- readFile file
    out @?= exp
  where
    url = "http://localhost:8081/" ++ endpoint
