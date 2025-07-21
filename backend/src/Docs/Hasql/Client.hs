module Docs.Hasql.Client
    ( hasqlClient
    ) where

import Hasql.Connection (Connection)
import Hasql.Session (SessionError)
import qualified Hasql.Session as Session

import Docs.Client (Client (..))
import qualified Docs.Hasql.Sessions as Sessions

-- | A document management client for a hasql connection.
hasqlClient :: Connection -> Client IO SessionError
hasqlClient conn =
    Client
        { createDocument = (session .) . Sessions.createDocument
        , getDocument = session . Sessions.getDocument
        , createTextElement = (session .) . Sessions.createTextElement
        , createTextRevision = (session .) . Sessions.createTextRevision
        }
  where
    session = (`Session.run` conn)
