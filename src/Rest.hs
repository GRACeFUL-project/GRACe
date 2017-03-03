{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Rest where

import GL hiding (Proxy)
import Library

import Control.Monad
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
{-import Data.Text (unpack, pack)-}
import qualified Data.ByteString.Lazy.Char8 as BS
import Servant
import Network.Wai.Handler.Warp (run)
import Servant.HTML.Lucid
import Lucid


data Res = Res Int deriving (Show, Eq)

instance ToJSON Res where
    toJSON (Res n) = object ["result" .= n]

type API   =   "library" :> Get '[JSON] Library
         :<|>  "submit"  :> Get '[JSON] Res

server :: Server API
server   =   return library
       :<|>  return (Res 42)

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8081 app


-- Test data

library :: Library
library = Library "crud" 
    [ Item "rain" $ 
         rain ::: tFloat "amount" .-> tGCM (tPort "rainfail" tFloat)
    ]

rain :: Float -> GCM (Port Float)
rain amount = do
  port <- createPort
  set port amount
  return port

pp :: ToJSON a => a -> IO ()
pp = BS.putStrLn . encodePretty
