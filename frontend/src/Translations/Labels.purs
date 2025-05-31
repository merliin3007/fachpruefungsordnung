module Translations.Labels where

import Prelude

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

-- Symbols should be in alphabetic order.
type Labels =
  ( "home"
      ::: "password"
      ::: "profile"
      ::: SNil
  )

en :: Translation Labels
en = fromRecord
  { password: "Password"
  , home: "Home"
  , profile: "Profile"
  }

de :: Translation Labels
de = fromRecord
  { password: "Passwort"
  , home: "Start"
  , profile: "Profil"
  }
