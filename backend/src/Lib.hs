{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import qualified Hasql.Session as Session
import Database (getConnection)
import qualified Database.Sessions as Sessions
import Server

someFunc :: IO ()
someFunc = do
  Right connection <- getConnection
  user <- Session.run (Sessions.getUser "test@test.com") connection
  print user
  runServer
  return ()
