module Main where
import System.Directory
import System.FilePath.Glob
import Text.ParserCombinators.Parsec.Char
import Text.Parsec
import Data.List
import System.Environment

-- | Read the library from a directory
readRawLibrary :: FilePath -> IO [String]
readRawLibrary libDir = do
  -- Get the .hs files in the library directory
  libDirHsFiles <- filter (match $ compile "*.hs") <$> listDirectory libDir

  -- Get the contents of the library
  libFilesContents <- withCurrentDirectory libDir 
                   $ sequence $ readFile <$> libDirHsFiles

  -- Return the contest
  return ["{" ++ s ++ "}" | Right s <- parse parseJSONComment "" <$> libFilesContents]

-- | Parse the {-% %-} blocks
parseJSONComment :: Parsec String () String
parseJSONComment = manyTill anyToken (string "{-%") >> manyTill anyToken (string "%-}")

-- | Turn a list of JSON blobs in to a JSON list of JSON things
libify :: [String] -> String
libify jsons = "{\"library\":[" ++ intercalate "," jsons ++ "]}"

-- | Produce a single JSON file called "library.json"
--   from a library
main = do
  args <- getArgs
  let dir = head args
  library <- libify <$> readRawLibrary dir
  writeFile "library.json" library
