module Test.Login where

import Prelude

import Affjax.ResponseHeader (ResponseHeader(..))
import Data.Maybe (Maybe(Just), isNothing)
import Effect (Effect)
import FPO.Page.Login (findXXsrfTokenInHeaders)
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

main :: Effect Unit
main = do
  quickCheck \xs ->
    let
      headers = map unwrap xs <> [ ResponseHeader "X-XSRF-TOKEN" "token" ]
    in
      eq (Just "token") (findXXsrfTokenInHeaders headers)
  quickCheck \xs ->
    let
      headers = map unwrap xs
    in
      isNothing (findXXsrfTokenInHeaders headers)

newtype TestHeader = TestHeader ResponseHeader

derive newtype instance eqTestHeader :: Eq TestHeader

instance Arbitrary TestHeader where
  arbitrary = do
    name <- arbitrary
    value <- arbitrary
    pure $ TestHeader
      (ResponseHeader (if name == "X-XSRF-TOKEN" then "otherName" else name) value)

unwrap :: TestHeader -> ResponseHeader
unwrap (TestHeader rh) = rh
