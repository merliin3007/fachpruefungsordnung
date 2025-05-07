{-# LANGUAGE OverloadedStrings #-}


import Servant
import Servant.Client hiding (manager, baseUrl)
import Test.Hspec
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp (testWithApplication, Port)

import Server (API, app)
import APIQuickCheck (followsBestPractices)

main :: IO ()
main = do 
        hspec testAPI               -- unit tests
        putStrLn "API QuickCheck is skipped"
        -- Currently not used because API is not following any rules 
        -- hspec followsBestPractices  -- quickcheck for all endpoints 


-- Modified Example from:
-- https://docs.servant.dev/en/stable/cookbook/testing/Testing.html#imports-and-our-testing-module


withUserApp :: (Port -> IO ()) -> IO ()
withUserApp action =
  -- testWithApplication makes sure the action is executed after the server has
  -- started and is being properly shutdown.
  testWithApplication (pure app) action



testAPI :: Spec
testAPI = 
  -- `around` will start the Server before the tests and turn it off after
  around withUserApp $ do
    let pingHandler :<|> _ = client (Proxy :: Proxy API)
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

    -- unit tests start here
    describe "GET /ping" $ do
      it "should return 'pong'" $ \port -> do
        result <- runClientM pingHandler (clientEnv port)
        result `shouldBe` (Right "pong")