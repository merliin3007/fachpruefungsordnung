{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server (runServer) where

import Control.Monad.IO.Class
import Data.Vector
import Database (getConnection)
import qualified Database.Sessions as Sessions
import qualified Database.User as User
import qualified Hasql.Session as Session
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type PingAPI = "ping" :> Get '[JSON] String

type UserAPI = "users" :> Get '[JSON] [User.User]

type API = PingAPI :<|> UserAPI

pingServer :: Server PingAPI
pingServer = return "pong"

userServer :: Server UserAPI
userServer = liftIO $ do
  Right connection <- getConnection
  Right vector <- Session.run Sessions.getUsers connection
  return $ toList vector

server :: Server API
server = pingServer :<|> userServer

testAPI :: Proxy API
testAPI = Proxy

app :: Application
app = serve testAPI server

runServer :: IO ()
runServer = run 80 app
