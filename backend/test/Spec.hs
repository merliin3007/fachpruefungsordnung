{-# LANGUAGE OverloadedStrings #-}

import APIQuickCheck (followsBestPractices)
import qualified Auth
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (Port, testWithApplication)
import Servant
import Servant.Auth.Server (generateKey)
import Servant.Client hiding (baseUrl, manager)
import Server
    ( PublicAPI
    , app
    , cookieSettings
    , jwtSettings
    )
import Test.Hspec

main :: IO ()
main = do
    putStrLn "Testing is skipped because database Mocking is not implemented yet!"

-- hspec testAPI               -- unit tests
-- hspec followsBestPractices  -- quickcheck for all endpoints

-- Modified Example from:
-- https://docs.servant.dev/en/stable/cookbook/testing/Testing.html#imports-and-our-testing-module

withUserApp :: (Port -> IO ()) -> IO ()
withUserApp action = do
    jwk <- generateKey
    -- testWithApplication makes sure the action is executed after the server has
    -- started and is being properly shutdown.
    testWithApplication (pure (app cookieSettings (jwtSettings jwk))) action

testAPI :: Spec
testAPI =
    -- `around` will start the Server before the tests and turn it off after
    around withUserApp $ do
        let pingHandler :<|> _ :<|> loginHandler :<|> _ = client (Proxy :: Proxy PublicAPI)
        baseUrl <- runIO $ parseBaseUrl "http://localhost"
        manager <- runIO $ newManager defaultManagerSettings
        let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

        -- unit tests start here
        describe "GET /ping" $ do
            it "should return 'pong'" $ \port -> do
                result <- runClientM pingHandler (clientEnv port)
                result `shouldBe` Right "pong"

        -- TODO: Mock DB for further testing
        describe "POST /login" $ do
            it "login gives returns NoContent" $ \port -> do
                let userDataLogin = Auth.UserLoginData "test" "123"
                response <- runClientM (loginHandler userDataLogin) (clientEnv port)
                case response of
                    Left err -> expectationFailure $ "Request failed: " ++ show err
                    Right (Headers body _headers) ->
                        body `shouldBe` NoContent
