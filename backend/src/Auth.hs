{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Auth (Token(..), UserLoginData(..), UserRegisterData(..)) where

import Servant.Auth.Server
import Data.Aeson
import GHC.Generics
import Data.Text
import Data.OpenApi (ToSchema)


newtype Token = Token { unToken :: Text }
  deriving (Generic, ToJSON, ToJWT, FromJSON, FromJWT)

data UserLoginData = UserLoginData {
    loginEmail :: Text
    , loginPassword :: Text
} deriving (Generic, FromJSON, ToSchema)

data UserRegisterData = UserRegisterData {
    registerName ::Text
    , registerEmail :: Text
    , registerPassword :: Text
} deriving (Generic, FromJSON, ToSchema)
