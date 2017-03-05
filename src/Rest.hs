{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Rest where

import Compile0
import GL hiding (Proxy)
import Library

import Control.Monad
import Data.Aeson hiding (Bool, String)
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types hiding (Bool, String)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M
import Servant
import Network.Wai.Handler.Warp (run)
import Servant.HTML.Lucid
import Lucid

data Res = Res Int deriving (Show, Eq)

instance ToJSON Res where
    toJSON (Res n) = object ["result" .= n]

type API   =   "library" :> Capture "name" String :> Get '[JSON, HTML] Library
         :<|>  "submit"  :> Get '[JSON] Res

server :: Server API
server   =   library
       :<|>  return (Res 42)

library :: String -> Handler Library
library n = case M.lookup n libraries of
    Just lib -> return lib
    Nothing  -> throwError $ err404 { errBody =  "No such lib" }

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8081 app


-- HTML rep
instance ToHtml Library where
    toHtml lib = tr_ $ do
        td_ (toHtml $ libraryId lib) 
    toHtmlRaw = toHtml

-- Test data

libraries :: M.Map String Library
libraries = M.fromList [(n, lib) | lib@(Library n _) <- [crud]]

crud :: Library
crud = Library "crud" 
    [ Item "rain" $ 
         rain ::: "amount" # tFloat .-> tGCM (tPort $ "rainfall" # tFloat)
    ]

rain :: Float -> GCM (Port Float)
rain amount = do
  port <- createPort
  set port amount
  return port

pp :: ToJSON a => a -> IO ()
pp = BS.putStrLn . encodePretty
