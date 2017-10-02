{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Compile0
import GRACeGraph
import Library hiding (Proxy, value)
import Submit

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (Bool, String)
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types hiding (Bool, String, Parser, Options)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M
import Data.Maybe
import Servant
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger
import Servant.HTML.Lucid
import System.Environment (getArgs)
import Lucid
import Language.Haskell.Interpreter hiding (set)
import System.Directory
import System.FilePath
import Options.Applicative
import Data.Semigroup ((<>))


-- Cross-Origin Resource Sharing (CORS) prevents browser warnings
-- about cross-site scripting
type Resp a = Headers '[Header "Access-Control-Allow-Origin" String] a

type API =
        "library" :> Capture "name" String             -- identifier of the library
                  :> Get '[JSON, HTML] (Resp Library)  -- return a library in eiterh JSON or HTML format

  :<|>  "submit"  :> Capture "lib"  String             -- library identifier
                  :> ReqBody '[JSON] Graph             -- Graph representation of a GCM model in JSON format
                  :> Post '[JSON] (Resp Value)         -- the result of the constraint solver in JSON
  :<|> "static" :> Raw                                    -- sets the path for raw call (direct call into a specified folder "/webapp")
  
  
type Libraries = M.Map String Library

data Options = Options
  { log    :: Bool
  , libdir :: FilePath
  } deriving Show

server :: Libraries -> Server API
server libs =    library libs
            :<|> submit libs
            :<|> serveDirectoryFileServer "/webapp" -- tells the static path where to look (location of the editor (index.html and java scripts)

hdr :: Handler a -> Handler (Resp a)
hdr h = h >>= return . addHeader "*"

library :: Libraries -> String -> Handler (Resp Library)
library libs = hdr . getLib libs

submit :: Libraries -> String -> Graph -> Handler (Resp Value)
submit libs n graph = hdr $ do
  lib <- getLib libs n
  out <- liftIO $ runGCM $ mkGCM (nodes graph) lib
  let res = fromMaybe Null $ decode $ BS.pack out
  return $ object ["result" .= res]

getLib :: Libraries -> String -> Handler Library
getLib libs n = case M.lookup n libs of
  Just lib -> return lib
  Nothing  -> throwError $ err404 { errBody = "No such lib" }

api :: Proxy API
api = Proxy

app :: Libraries -> Application
app libs = serve api (server libs)

main :: IO ()
main = do
  Options log dir <- execParser parser
  putStrLn "Loading libraries..."
  libs            <- loadLibraries dir
  putStrLn "Done! Starting GRACeServer on port 8081."
  run 8081 $ if log
    then logStdoutDev $ app libs
    else app libs
 where
  -- Usage information and options parser
  parser = info (flags <**> helper)
      ( fullDesc
     <> progDesc "GRACe is a DSL for expressing concept maps of system dynamics \
                 \models that can be evaluated by a constraint solver."
     <> header "GRACeServer - a server for evaluating GRACe programs"
      )

  -- Options parser
  flags = Options <$>
      switch
      ( long "log"
     <> help "Turn logging on"
      )
    <*> strOption
      ( long "lib"
     <> short 'l'
     <> help "File path to library modules."
     <> showDefault
     <> value "libraries"
      )

-- Load all libraries in given directory
loadLibraries :: FilePath -> IO Libraries
loadLibraries dir = do
  fs   <- listDirectory dir >>= return . filter (\l -> takeExtension l == ".hs")
  libs <- withCurrentDirectory dir $ mapM (runInterpreter . loadLib) fs
  mapM (\l -> putStrLn $ "Found library: " ++ l) [ libraryId l | Right l <- libs ]
  return $ M.fromList [(libraryId l, l) | Right l <- libs]
 where
  loadLib :: String -> Interpreter Library
  loadLib lib = do
    loadModules [lib]
    setTopLevelModules [takeWhile (/='.') lib]
    interpret "library" (as :: Library)

-- HTML rep
instance ToHtml Library where
    toHtml lib = tr_ $ do
        td_ (toHtml $ libraryId lib)
    toHtmlRaw = toHtml

-- Debug functions
pp :: ToJSON a => a -> IO ()
pp = BS.putStrLn . encodePretty
