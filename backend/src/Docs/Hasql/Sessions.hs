module Docs.Hasql.Sessions
    ( createDocument
    , getDocument
    , createTextRevision
    ) where

import Data.Text (Text)

import Hasql.Session (Session, statement)
import Hasql.Transaction.Sessions
    ( IsolationLevel (..)
    , Mode (..)
    , transaction
    )

import Docs.Document (Document, DocumentID)
import qualified Docs.Hasql.Statements as Statements
import qualified Docs.Hasql.Transactions as Transactions
import Docs.TextRevision
    ( NewTextRevision
    , TextRevision
    , TextRevisionConflict
    )
import Docs.Util (UserID)
import UserManagement.Group (GroupID)

createDocument :: Text -> GroupID -> Session Document
createDocument = curry (`statement` Statements.createDocument)

getDocument :: DocumentID -> Session (Maybe Document)
getDocument = (`statement` Statements.getDocument)

createTextRevision
    :: UserID
    -> NewTextRevision
    -> Session (Either TextRevisionConflict TextRevision)
createTextRevision userID newRevision =
    transaction
        Serializable
        Write
        $ Transactions.createTextRevision userID newRevision
