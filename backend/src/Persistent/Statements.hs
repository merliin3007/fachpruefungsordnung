{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Persistent.Statements
  ( getUser,
    getUsers,
    putUser,
    putBlob,
  )
where

import Data.Profunctor (lmap, rmap)
import Data.Text
import Data.Tuple.Curry (uncurryN)
import Data.Vector
import GHC.Int
import Hasql.Statement
import Hasql.TH
import qualified Persistent.User as User

getUsers :: Statement () (Vector User.User)
getUsers =
  rmap
    (fmap (uncurryN User.User))
    [vectorStatement|
      select name :: text, email :: text?
      from users
    |]

getUser :: Statement Int32 (Maybe User.User)
getUser =
  rmap
    (fmap (uncurryN User.User))
    [maybeStatement|
     select name :: text, email :: text?
     from users
     where id = $1 :: int
   |]

putUser :: Statement User.User Int32
putUser =
  lmap
    (\(User.User name email) -> (name, email))
    [singletonStatement|
      insert into users (name, email)
      values ($1 :: text, $2 :: text?)
      returning id :: int4
    |]

putBlob :: Statement (Text, Text) ()
putBlob =
  [singletonStatement|
    insert into blobs (hash, content) values ($1 :: text, $2 :: text)
  |]
