-- | This module defines the Store type and the actions that can be performed on it.
-- | Using the Store type, we can manage the state of the application and store various
-- | information such as account information, user data, and other relevant local data.

module FPO.Data.Store where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import FPO.Data.Route (Route)
import Translations.Translator (EqTranslator)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem) as LocalStorage

type User = { userName :: String, isAdmin :: Boolean }

-- | The Store type represents the global state of the application.
type Store =
  { inputMail :: String -- ^ The email that was input in the login form (example state variable)
  , user :: Maybe User -- ^ The user's name
  , loginRedirect :: Maybe Route -- ^ The route to redirect to after login
  , translator :: EqTranslator
  , language :: String
  }

data Action
  = SetMail String -- ^ Action to set the user's email.
  | SetUser (Maybe User) -- ^ Action to set the user's name.
  | SetLoginRedirect (Maybe Route) -- ^ Action to set the redirect route after login.
  | SetLanguage String

-- | Update the store based on the action.
reduce :: Store -> Action -> Store
reduce store = case _ of
  SetMail m -> store { inputMail = m }
  SetUser u -> store { user = u }
  -- TODO: Using `Store.loginRedirect`, we can specify the route to redirect to after login.
  --       While sufficent for the case of logins and login redirect handling, this is not
  --       flexible (i.e., usable for pages and actions other than login).
  --       A more scalable approach would be to encode the redirect route in the login's query
  --       parameters, but the Route type must not be cyclic, so we cannot use the Route type
  --       directly.
  --       We could also rename this action to `SetRedirect` and use it for all redirects,
  --       but this way, `NavigateQ` cannot take care of clearing the redirect route
  --       when navigating to a different page (in general) and care must be taken regarding
  --       (re)setting the redirect route.
  SetLoginRedirect r -> store { loginRedirect = r }
  SetLanguage s -> store { language = s }

saveLanguage :: String -> Effect Unit
saveLanguage lang = do
  win <- window
  storage <- localStorage win
  LocalStorage.setItem "userLanguage" lang storage

loadLanguage :: Effect (Maybe String)
loadLanguage = do
  win <- window
  storage <- localStorage win
  LocalStorage.getItem "userLanguage" storage
