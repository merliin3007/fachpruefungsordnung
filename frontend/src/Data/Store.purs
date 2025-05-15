-- | This module defines the Store type and the actions that can be performed on it.
-- | 
-- | Using the Store type, we can manage the state of the application.
-- |
-- | This interface is not yet used, but it is a good starting point for managing
-- | the state of the application.

module FPO.Data.Store where

import Data.Maybe (Maybe(..))

-- | The Store type represents the global state of the application.
type Store =
  { userMail :: Maybe String -- ^ The user's email (example state variable)
  }

data Action = SetUserMail String -- ^ Action to set the user name

-- | Update the store based on the action.
reduce :: Store -> Action -> Store
reduce store = case _ of
  SetUserMail mail -> store { userMail = Just mail }