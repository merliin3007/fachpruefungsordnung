module VersionControl
    ( Context (..)
    , createCommit
    , getCommit
    ) where

import Control.Arrow (left)
import Data.Functor ((<&>))
import Hasql.Connection (Connection)
import Hasql.Session (Session, SessionError)
import qualified Hasql.Session as Session
import VersionControl.Commit (CommitID, CreateCommit, ExistingCommit)
import VersionControl.Error (VersionControlError (..))
import qualified VersionControl.Sessions as Sessions

-- | context for the version control (currently just a database connection)
newtype Context = Context
    { unConnection :: Connection
    }

-- | creates a commit in the given version control context
createCommit
    :: CreateCommit
    -> Context
    -> IO (Either VersionControlError ExistingCommit)
createCommit = runSession . Sessions.createCommit

-- | gets a commit from the given version control context
getCommit
    :: CommitID
    -> Context
    -> IO (Either VersionControlError ExistingCommit)
getCommit = runSession . Sessions.getCommit

-- | runs a session and maps a potential error to a `VersionControlError`
runSession :: Session a -> Context -> IO (Either VersionControlError a)
runSession session ctx =
    Session.run
        session
        (unConnection ctx)
        <&> mapResult

-- | maps a potential error to a `VersionControlError`
mapResult :: Either SessionError a -> Either VersionControlError a
mapResult = left (DatabaseError . show)
