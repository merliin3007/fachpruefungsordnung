module APIQuickCheck (followsBestPractices) where

import Server (API, server)

import Servant
import Test.Hspec
import Servant.QuickCheck
import Test.QuickCheck (stdArgs)


testServer :: IO (Server API)
testServer = return server


followsBestPractices :: Spec
followsBestPractices = describe "API" $ do

  it "follows best practices" $ do
    withServantServer (Proxy :: Proxy API) testServer $ \burl ->
      serverSatisfies (Proxy :: Proxy API) burl stdArgs
           ( not500                  -- no internal server error 500
         <%> onlyJsonObjects         -- all responses are in JSON format
         <%> getsHaveCacheControlHeader
        --  <%> headsHaveCacheControlHeader
         <%> mempty)