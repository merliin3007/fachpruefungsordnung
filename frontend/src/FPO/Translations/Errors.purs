module FPO.Translations.Errors
  ( deErrors
  , enErrors
  ) where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type ErrorLabels =
  ( "error_accessDeniedError"
      ::: "error_authError"
      ::: "error_dataError"
      ::: "error_methodNotAllowedError"
      ::: "error_networkError"
      ::: "error_notFoundError"
      ::: "error_serverError"
      ::: SNil
  )

enErrors :: Translation ErrorLabels
enErrors = fromRecord
  { error_accessDeniedError: "Access denied"
  , error_authError: "Authentication error: "
  , error_dataError: "Data error: "
  , error_methodNotAllowedError: "Method not allowed: "
  , error_networkError: "Network error: "
  , error_notFoundError: "Not found: "
  , error_serverError: "Server error: "
  }

deErrors :: Translation ErrorLabels
deErrors = fromRecord
  { error_accessDeniedError: "Zugriff verweigert"
  , error_authError: "Authentifizierungsfehler: "
  , error_dataError: "Datenfehler: "
  , error_methodNotAllowedError: "Methode nicht erlaubt: "
  , error_networkError: "Netzwerkfehler: "
  , error_notFoundError: "Nicht gefunden: "
  , error_serverError: "Serverfehler: "
  }
