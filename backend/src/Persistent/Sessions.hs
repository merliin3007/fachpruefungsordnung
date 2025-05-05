module Persistent.Sessions (getUsers, getUser, putUser, putBlob) where

import Data.Text
import Data.Vector
import GHC.Int
import Hasql.Session (Session, statement)
import qualified Persistent.Statements as Statements
import qualified Persistent.User as User
import Persistent.Utility (compressText, hashText)

getUsers :: Session (Vector User.User)
getUsers = statement () Statements.getUsers

getUser :: Int32 -> Session (Maybe User.User)
getUser userId = statement userId Statements.getUser

putUser :: User.User -> Session Int32
putUser user = statement user Statements.putUser

putBlob :: Text -> Session Text
putBlob content = do
  let hash = hashText content
  statement (hash, compressText content) Statements.putBlob
  return hash
