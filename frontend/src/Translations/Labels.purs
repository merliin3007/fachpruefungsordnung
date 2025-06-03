module Translations.Labels where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

-- Symbols MUST be in alphabetic order.
type Labels =
  ( "home"
      ::: "loginSuccessful"
      ::: "password"
      ::: "profile"
      ::: "role"
      ::: "userData"
      ::: "userName"
      ::: SNil
  )

en :: Translation Labels
en = fromRecord
  { password: "Password"
  , home: "Home"
  , profile: "Profile"
  , userData: "User data"
  , userName: "User name"
  , role: "Role"
  , loginSuccessful: "Login successful"
  }

de :: Translation Labels
de = fromRecord
  { password: "Passwort"
  , home: "Start"
  , profile: "Profil"
  , userData: "Benutzerdaten"
  , userName: "Benutzername"
  , role: "Rolle"
  , loginSuccessful: "Login erfolgreich"
  }
