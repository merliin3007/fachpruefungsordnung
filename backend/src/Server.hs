{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server (runServer, DocumentedAPI, PublicAPI, jwtSettings, cookieSettings, app, server) where

import qualified Auth
import Control.Lens
import Control.Monad.IO.Class
import Crypto.JOSE.JWK (JWK)
import Data.OpenApi
    ( OpenApi
    , description
    , info
    , license
    , servers
    , title
    , version
    )
import Data.Password.Argon2
import qualified Data.Text as T
import Data.Vector
import Database (getConnection)
import GHC.Int
import qualified Hasql.Session as Session
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server
import Servant.OpenApi
import qualified UserManagement.Sessions as Sessions
import qualified UserManagement.User as User
import Versioning.Commit
import qualified Versioning.Sessions as VSessions

type DebugAPI =
    "commits" :> Capture "id" Int32 :> Get '[JSON] ExistingCommit

type PublicAPI =
    "ping" :> Get '[JSON] String
        :<|> "users" :> Get '[JSON] [User.User]
        :<|> DebugAPI
        :<|> "login"
            :> ReqBody '[JSON] Auth.UserLoginData
            :> Post
                '[JSON]
                ( Headers
                    '[ Header "Set-Cookie" SetCookie
                     , Header "Set-Cookie" SetCookie
                     ]
                    NoContent
                )
        :<|> "register"
            :> ReqBody '[JSON] Auth.UserRegisterData
            :> Post '[JSON] NoContent

type ProtectedAPI auths =
    Auth auths Auth.Token :> "protected" :> Get '[JSON] String

type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi

type DocumentedAPI auths = SwaggerAPI :<|> PublicAPI :<|> ProtectedAPI auths

pingHandler :: Handler String
pingHandler = return "pong"

getCommitHandler :: Int32 -> Handler ExistingCommit
getCommitHandler id' = liftIO $ do
    Right connection <- getConnection
    Right commit <- Session.run (VSessions.getCommit (CommitID id')) connection
    return commit

debugAPIHandler :: Int32 -> Handler ExistingCommit
debugAPIHandler = getCommitHandler

protectedHandler :: AuthResult Auth.Token -> Handler String
protectedHandler (Authenticated Auth.Token {..}) =
    return $ "This is very private content of " <> T.unpack unToken <> "!"
protectedHandler _ =
    throwError
        err403
            { errBody = "Not allowed! You need to login to see this content.\n"
            }

userHandler :: Handler [User.User]
userHandler = liftIO $ do
    Right connection <- getConnection
    Right vector <- Session.run Sessions.getUsers connection
    return $ toList vector

loginHandler
    :: CookieSettings
    -> JWTSettings
    -> Auth.UserLoginData
    -> Handler
        ( Headers
            '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
            NoContent
        )
loginHandler cookieSett jwtSett Auth.UserLoginData {..} = do
    eConn <- liftIO getConnection
    case eConn of
        Left _ -> throwError $ err401 {errBody = "login failed! Please try again!\n"}
        Right conn -> do
            eUser <- liftIO $ Session.run (Sessions.getUser loginEmail) conn
            case eUser of
                Right (Just User.User {..}) -> do
                    let passwordCheck = checkPassword (mkPassword loginPassword) (PasswordHash pwhash)
                    case passwordCheck of
                        PasswordCheckFail -> throwError $ err401 {errBody = "email or password incorrect\n"}
                        PasswordCheckSuccess -> do
                            mLoginAccepted <-
                                liftIO $ acceptLogin cookieSett jwtSett (Auth.Token loginEmail)
                            case mLoginAccepted of
                                Nothing -> throwError $ err401 {errBody = "login failed! Please try again!\n"}
                                Just addHeaders -> return $ addHeaders NoContent
                _ -> throwError $ err401 {errBody = "login failed! Please try again!\n"}

registerHandler :: Auth.UserRegisterData -> Handler NoContent
registerHandler Auth.UserRegisterData {..} = do
    eConn <- liftIO getConnection
    case eConn of
        Left _ -> throwError $ err401 {errBody = "registration failed! Please try again!\n"}
        Right conn -> do
            eUser <- liftIO $ Session.run (Sessions.getUser registerEmail) conn
            case eUser of
                Right Nothing -> do
                    PasswordHash hashedText <- liftIO $ hashPassword $ mkPassword registerPassword
                    eAction <-
                        liftIO $
                            Session.run
                                ( Sessions.putUser
                                    ( User.User
                                        registerName
                                        registerEmail
                                        hashedText
                                    )
                                )
                                conn
                    case eAction of
                        Left _ -> throwError $ err401 {errBody = "registration failed! Please try again!\n"}
                        Right _ -> do
                            return NoContent
                _ -> throwError $ err401 {errBody = "registration failed! Please try again!\n"}

api :: Proxy PublicAPI
api = Proxy

swagger :: OpenApi
swagger =
    toOpenApi api
        & info . title .~ "Fachprüfungsordnung API"
        & info . version .~ "1.0"
        & info . description ?~ "This is the API for the Fachprüfungsordnung editor."
        & info . license ?~ "AGPL3"
        & servers .~ ["https://batailley.informatik.uni-kiel.de/api/"]

server :: CookieSettings -> JWTSettings -> Server (DocumentedAPI auths)
server cookieSett jwtSett =
    return swagger
        :<|> ( pingHandler
                :<|> userHandler
                :<|> debugAPIHandler
                :<|> loginHandler cookieSett jwtSett
                :<|> registerHandler
             )
        :<|> protectedHandler

documentedAPI :: Proxy (DocumentedAPI '[JWT, Cookie])
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
