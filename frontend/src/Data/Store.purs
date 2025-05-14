-- | This module defines the Store type and the actions that can be performed on it.
-- | 
-- | Using the Store type, we can manage the state of the application.
-- |
-- | This interface is not yet used, but it is a good starting point for managing
-- | the state of the application.

module FPO.Data.Store where

import Data.Maybe (Maybe(..))

type Store =
  { currentUser :: Maybe String
  }

data Action
  = LoginUser String
  | LogoutUser

reduce :: Store -> Action -> Store
reduce store = case _ of
  LoginUser user -> store { currentUser = Just user }
  LogoutUser -> store { currentUser = Nothing }