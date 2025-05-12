{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module APIQuickCheck (followsBestPractices) where

import qualified Auth
import Control.Exception (throw)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Crypto.JOSE.JWK (JWK)
import Data.Aeson
import Data.Text (Text, pack)
import Network.HTTP.Client (httpLbs, responseStatus)
import Network.HTTP.Types (status500)
import Servant
import Servant.Auth.Server
import Servant.QuickCheck
import Servant.QuickCheck.Internal.ErrorTypes (PredicateFailure (..))
import Server (DocumentedAPI, cookieSettings, jwtSettings, server)
import Test.Hspec hiding (context)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, stdArgs)

instance ToJSON Auth.UserLoginData

instance ToJSON Auth.UserRegisterData

instance Arbitrary Auth.UserLoginData where
  arbitrary =
    Auth.UserLoginData
      <$> arbitrary
      <*> arbitrary

instance Arbitrary Auth.UserRegisterData where
  arbitrary =
    Auth.UserRegisterData
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

type Auths = '[JWT, Cookie]

-- | custom not500 predicate that also prints the failed request
betterNot500 :: RequestPredicate
betterNot500 = RequestPredicate $ \request manager -> do
  response <- httpLbs request manager
  when (responseStatus response == status500) $
    throw $
      PredicateFailure "betterNot500" (Just request) response
  return []

testServer :: JWK -> IO (Server (DocumentedAPI Auths))
testServer jwk = return $ server cookieSettings (jwtSettings jwk)

followsBestPractices :: Spec
followsBestPractices = describe "API" $ do
  it "follows best practices" $ do
    jwk <- liftIO generateKey
    let jwtSett = jwtSettings jwk
    let context = (cookieSettings :. jwtSett :. EmptyContext) :: (Context '[CookieSettings, JWTSettings])

    withServantServerAndContext
      (Proxy :: Proxy (DocumentedAPI Auths))
      context
      (testServer jwk)
      $ \burl ->
        serverSatisfies
          (Proxy :: Proxy (DocumentedAPI Auths))
          burl
          stdArgs
          ( betterNot500 -- no internal server error 500
          -- <%> onlyJsonObjects         -- all responses are in JSON format
              <%> getsHaveCacheControlHeader
              --  <%> headsHaveCacheControlHeader
              <%> mempty
          )
