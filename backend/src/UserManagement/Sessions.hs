module UserManagement.Sessions
  ( getUsers,
    getUser,
    putUser,
  )
where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified UserManagement.Statements as Statements
import qualified UserManagement.User as User
import GHC.Int
import Hasql.Session (Session, statement)

getUsers :: Session (Vector User.User)
getUsers = statement () Statements.getUsers

getUser :: Text -> Session (Maybe User.User)
getUser userEmail = statement userEmail Statements.getUser

putUser :: User.User -> Session Int32
putUser user = statement user Statements.putUser
