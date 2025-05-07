{-# LANGUAGE DeriveGeneric #-}

module Database.User
  ( User (..),
  )
where

import Data.Aeson
import Data.Text
import GHC.Generics

data User = User
  { name :: Text,
    email :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User