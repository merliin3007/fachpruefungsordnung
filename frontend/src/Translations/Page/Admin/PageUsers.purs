module Translations.Page.Admin.PageUsers where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type AdminUserPageLabels =
  ( "admin_users_create"
      ::: "admin_users_createNewUser"
      ::: "admin_users_listOfUsers"
      ::: SNil
  )

enAdminUserPage :: Translation AdminUserPageLabels
enAdminUserPage = fromRecord
  { admin_users_listOfUsers: "List of Users"
  , admin_users_createNewUser: "Create New User"
  , admin_users_create: "Create"
  }

deAdminUserPage :: Translation AdminUserPageLabels
deAdminUserPage = fromRecord
  { admin_users_listOfUsers: "Liste der Nutzer"
  , admin_users_createNewUser: "Neuen Nutzer erstellen"
  , admin_users_create: "Erstellen"
  }

