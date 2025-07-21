module Docs.Hasql.Sessions
    ( createDocument
    , getDocument
    , createTextElement
    , createTextRevision
    , getTextElementRevision
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
import Docs.TextElement (TextElement, TextElementKind)
import Docs.TextRevision
    ( NewTextRevision
    , TextElementRevision
    , TextRevision
    , TextRevisionConflict
    , TextRevisionSelector
    )
import qualified Docs.TextRevision as TextRevision
import Docs.Util (UserID)
import UserManagement.Group (GroupID)

createDocument :: Text -> GroupID -> Session Document
createDocument = curry (`statement` Statements.createDocument)

getDocument :: DocumentID -> Session (Maybe Document)
getDocument = (`statement` Statements.getDocument)

createTextElement :: DocumentID -> TextElementKind -> Session TextElement
createTextElement = curry (`statement` Statements.createTextElement)

createTextRevision
    :: UserID
    -> NewTextRevision
    -> Session (Either TextRevisionConflict TextRevision)
createTextRevision userID newRevision =
    transaction
        Serializable
        Write
        $ Transactions.createTextRevision userID newRevision

getTextElementRevision
    :: TextRevisionSelector
    -> Session (Maybe TextElementRevision)
getTextElementRevision (TextRevision.Latest textElementID) =
    statement textElementID Statements.getLatestTextElementRevision
getTextElementRevision (TextRevision.Specific textRevisionID) =
    statement textRevisionID Statements.getTextElementRevision
