module Lib
  ( someFunc,
  )
where

import qualified Hasql.Session as Session
import Persistent (getConnection)
import qualified Persistent.Sessions as Sessions
import Server

someFunc :: IO ()
someFunc = do
  Right connection <- getConnection
  user <- Session.run (Sessions.getUser 1) connection
  print user
  runServer
  return ()
