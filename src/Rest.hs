{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Compile0
import GL hiding (Proxy)
import GRACeGraph
import Library
import Submit

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (Bool, String)
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types hiding (Bool, String)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M
import Servant
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger
import Servant.HTML.Lucid
import Lucid

data Res = Res String deriving (Show, Eq)

instance ToJSON Res where
    toJSON (Res x) = object ["result" .= x]

type API   =   "library" :> Capture "name" String :> Get  '[JSON, HTML] Library
         :<|>  "submit"  :> ReqBody '[JSON] Graph :> Post '[JSON] Res

server :: Server API
server   =   library
       :<|>  submit

library :: String -> Handler Library
library n = case M.lookup n libraries of
    Just lib -> return lib
    Nothing  -> throwError $ err404 { errBody =  "No such lib" }

submit :: Graph -> Handler Res
submit graph = do 
    out <- liftIO $ runGCM $ mkGCM (nodes graph) crud
    return $ Res $ process out
  where
    brackets s = "[" ++ s ++ "]"
    process = brackets . unlines . init . lines

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8081 $ logStdoutDev app

-- HTML rep
instance ToHtml Library where
    toHtml lib = tr_ $ do
        td_ (toHtml $ libraryId lib) 
    toHtmlRaw = toHtml

-- Test data

testLibrary file = do
  Just gr <- (decode . BS.pack) <$> readFile file
  return $ mkGCM (nodes gr) crud

libraries :: M.Map String Library
libraries = M.fromList [(n, lib) | lib@(Library n _) <- [crud]]

-- example stuff
crud :: Library
crud = Library "crud"
    [ Item "rain" $
         rain ::: "amount" # tFloat .-> tGCM ("rainfall" # tPort tFloat)

    , Item "pump" $
        pump ::: "capacity" # tFloat.-> tGCM (tPair ("inflow" # tPort tFloat)
                                                    ("outflow" # tPort tFloat))
    , Item "runoff area" $
        runoffArea ::: "storage capacity" # tFloat .-> tGCM (tTuple3 ("inflow" # tPort tFloat)
                                                                     ("outlet" # tPort tFloat)
                                                                     ("overflow" # tPort tFloat))
    ]

rain :: Float -> GCM (Port Float)
rain amount = do
  port <- createPort
  set port amount
  return port

pump :: Float -> GCM (Port Float, Port Float)
pump maxCap = do
  inPort  <- createPort
  outPort <- createPort

  component $ do
    inflow <- value inPort
    outflow <- value outPort

    assert $ inflow === outflow
    assert $ inflow `inRange` (0, lit maxCap)

  return (inPort, outPort)

runoffArea :: Float -> GCM (Port Float, Port Float, Port Float)
runoffArea cap = do
  inflow <- createPort
  outlet <- createPort
  overflow <- createPort

  component $ do
    currentStored <- createLVar

    inf <- value inflow
    out <- value outlet
    ovf <- value overflow
    sto <- value currentStored

    assert $ sto === inf - out - ovf
    assert $ sto `inRange` (0, lit cap)
    assert $ (ovf .> 0) ==> (sto === lit cap)
    assert $ ovf .>= 0

  return (inflow, outlet, overflow)

pp :: ToJSON a => a -> IO ()
pp = BS.putStrLn . encodePretty
