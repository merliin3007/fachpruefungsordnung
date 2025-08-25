{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server (runServer, DocumentedAPI, PublicAPI, jwtSettings, cookieSettings, app, server) where

import Control.Lens
import Control.Monad.IO.Class
import Crypto.JOSE.JWK (JWK)
import Data.ByteString.Lazy (readFile)
import Data.Maybe (fromMaybe)
import Data.OpenApi
    ( OpenApi
    , description
    , info
    , license
    , servers
    , title
    , version
    )
import Data.Time (UTCTime)
import Data.UUID (toString)
import qualified Docs
import qualified Docs.Hasql.Database as DB
import GHC.Int (Int64)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Server.Auth (AuthMethod)
import qualified Server.Auth as Auth
import Server.DTOs.Logs (Logs (Logs))
import qualified Server.DTOs.Logs as Logs
import Server.Handlers.AuthHandlers
import Server.Handlers.DocsHandlers (DocsAPI, docsServer, getUser, withDB)
import Server.Handlers.GroupHandlers
import Server.Handlers.PasswordResetHandlers
import Server.Handlers.RenderHandlers
import Server.Handlers.RoleHandlers
import Server.Handlers.UserHandlers
import Prelude hiding (readFile)

type PublicAPI =
    "ping" :> Get '[JSON] String
        :<|> "document" :> Get '[PDF] PDFByteString
        :<|> AuthAPI
        :<|> PasswordResetAPI

type ProtectedAPI =
    Auth AuthMethod Auth.Token
        :> "protected"
        :> Get '[JSON] String
        :<|> UserAPI
        :<|> GroupAPI
        :<|> RoleAPI
        :<|> DocsAPI
        :<|> RenderAPI
        :<|> LogsAPI

type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi

type DocumentedAPI = SwaggerAPI :<|> PublicAPI :<|> ProtectedAPI

type LogsAPI =
    Auth AuthMethod Auth.Token
        :> "logs"
        :> QueryParam "before" UTCTime
        :> QueryParam "limit" Int64
        :> Get '[JSON] Logs

logsHandler
    :: AuthResult Auth.Token
    -> Maybe UTCTime
    -> Maybe Int64
    -> Handler Logs
logsHandler auth offset limit = do
    userID <- getUser auth
    let limit' = fromMaybe 20 limit
    messages <- withDB $ DB.run $ Docs.getLogs userID offset limit'
    return $
        Logs
            { Logs.messages = messages
            , Logs.limit = limit'
            , Logs.offset = offset
            }

pingHandler :: Handler String
pingHandler = return "pong"

documentHandler :: Handler PDFByteString
documentHandler = liftIO $ do
    bs <- readFile "static/dummy.pdf"
    return $ PDFByteString bs

protectedHandler :: AuthResult Auth.Token -> Handler String
protectedHandler (Authenticated Auth.Token {..}) =
    return $ "This is very private content of " <> toString subject <> "!"
protectedHandler _ =
    throwError
        err403
            { errBody = "Not allowed! You need to login to see this content.\n"
            }

api :: Proxy (PublicAPI :<|> ProtectedAPI)
api = Proxy

swagger :: OpenApi
swagger =
    toOpenApi api
        & info . title .~ "Fachprüfungsordnung API"
        & info . version .~ "1.0"
        & info . description ?~ "This is the API for the Fachprüfungsordnung editor."
        & info . license ?~ "AGPL3"
        & servers
            .~ ["https://batailley.informatik.uni-kiel.de/api/", "http://localhost:8080/api/"]

server :: CookieSettings -> JWTSettings -> Server DocumentedAPI
server cookieSett jwtSett =
    return swagger
        :<|> ( pingHandler
                :<|> documentHandler
                :<|> authServer cookieSett jwtSett
                :<|> passwordResetServer
             )
        :<|> ( protectedHandler
                :<|> userServer
                :<|> groupServer
                :<|> roleServer
                :<|> docsServer
                :<|> renderServer
                :<|> logsHandler
             )

documentedAPI :: Proxy DocumentedAPI
documentedAPI = Proxy

app :: CookieSettings -> JWTSettings -> Application
app cookieSett jwtSett =
    serveWithContext
        documentedAPI
        (cookieSett :. jwtSett :. EmptyContext)
        (server cookieSett jwtSett)

jwtSettings :: JWK -> JWTSettings
jwtSettings = defaultJWTSettings

cookieSettings :: CookieSettings
cookieSettings = defaultCookieSettings

runServer :: IO ()
runServer = do
    let port = 80
    jwtSecretKey <- generateKey
    let jwtSett = jwtSettings jwtSecretKey
    run port (app cookieSettings jwtSett)
