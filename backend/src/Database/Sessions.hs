module Database.Sessions (getUsers, getUser, putUser, putBlob) where

import Data.Text
import Data.Vector
import GHC.Int
import Hasql.Session (Session, statement)
import qualified Database.Statements as Statements
import qualified Database.User as User
import Database.Utility (compressText, hashText)

getUsers :: Session (Vector User.User)
getUsers = statement () Statements.getUsers

getUser :: Text -> Session (Maybe User.User)
getUser userEmail = statement userEmail Statements.getUser

putUser :: User.User -> Session Int32
putUser user = statement user Statements.putUser

putBlob :: Text -> Session Text
putBlob content = do
  let hash = hashText content
  statement (hash, compressText content) Statements.putBlob
  return hash
