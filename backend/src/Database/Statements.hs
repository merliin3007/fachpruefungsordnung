{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Database.Statements
  ( getUser,
    getUsers,
    putUser,
  )
where

import Data.Profunctor (lmap, rmap)
import Data.Text
import Data.Tuple.Curry (uncurryN)
import Data.Vector
import qualified Database.User as User
import GHC.Int
import Hasql.Statement
import Hasql.TH

getUsers :: Statement () (Vector User.User)
getUsers =
  rmap
    (fmap (uncurryN User.User))
    [vectorStatement|
      select name :: text, email :: text, pwhash :: text
      from users
    |]

getUser :: Statement Text (Maybe User.User)
getUser =
  rmap
    (fmap (uncurryN User.User))
    [maybeStatement|
     select name :: text, email :: text, pwhash :: text
     from users
     where email = $1 :: text
   |]

putUser :: Statement User.User Int32
putUser =
  lmap
    (\(User.User name email pwhash) -> (name, email, pwhash))
    [singletonStatement|
      insert into users (name, email, pwhash)
      values ($1 :: text, $2 :: text, $3 :: text)
      returning id :: int4
    |]
