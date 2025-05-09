{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server (runServer, API, app, server) where

import Control.Lens
import Control.Monad.IO.Class
import Data.OpenApi (OpenApi, description, info, license, servers, title, version)
import Data.Vector
import Database (getConnection)
import qualified Database.Sessions as Sessions
import qualified Database.User as User
import qualified Hasql.Session as Session
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.OpenApi

type PingAPI = "ping" :> Get '[JSON] String

type UserAPI = "users" :> Get '[JSON] [User.User]

type API = PingAPI :<|> UserAPI

type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi

type DocumentedAPI = SwaggerAPI :<|> API

pingServer :: Server PingAPI
pingServer = return "pong"

userServer :: Server UserAPI
userServer = liftIO $ do
  Right connection <- getConnection
  Right vector <- Session.run Sessions.getUsers connection
  return $ toList vector

api :: Proxy API
api = Proxy

swagger :: OpenApi
swagger =
  toOpenApi api
    & info . title .~ "Fachprüfungsordnung API"
    & info . version .~ "1.0"
    & info . description ?~ "This is the API for the Fachprüfungsordnung editor."
    & info . license ?~ "AGPL3"
    & servers .~ ["https://batailley.informatik.uni-kiel.de/api/"]

server :: Server DocumentedAPI
server = return swagger :<|> pingServer :<|> userServer

documentedAPI :: Proxy DocumentedAPI
documentedAPI = Proxy

app :: Application
app = serve documentedAPI server

runServer :: IO ()
runServer = run 80 app
