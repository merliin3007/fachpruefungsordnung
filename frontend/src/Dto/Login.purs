module Dto.Login where

import Prelude

type LoginDto = { loginEmail :: String, loginPassword :: String }
type RegisterDto =
  { registerEmail :: String
  , registerName :: String
  , registerPassword :: String
  }

