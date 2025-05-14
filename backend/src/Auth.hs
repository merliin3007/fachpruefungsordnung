{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Auth (Token (..), UserLoginData (..), UserRegisterData (..)) where

import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Text
import GHC.Generics
import Servant.Auth.Server

newtype Token = Token {unToken :: Text}
    deriving (Generic, ToJSON, ToJWT, FromJSON, FromJWT)

data UserLoginData = UserLoginData
    { loginEmail :: Text
    , loginPassword :: Text
    }
    deriving (Generic, FromJSON, ToSchema)

data UserRegisterData = UserRegisterData
    { registerName :: Text
    , registerEmail :: Text
    , registerPassword :: Text
    }
    deriving (Generic, FromJSON, ToSchema)
