module Docs.Hasql.Sessions
    ( createDocument
    , getDocument
    , createTextElement
    , createTextRevision
    , getTextElementRevision
    , createTreeRevision
    , getTreeRevision
    , getTree
    , getTextRevisionHistory
    , getTreeRevisionHistory
    , getDocumentRevisionHistory
    ) where

import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Vector as Vector

import Hasql.Session (Session, statement)
import Hasql.Transaction.Sessions
    ( IsolationLevel (..)
    , Mode (..)
    , transaction
    )

import Docs.Document (Document, DocumentID)
import Docs.DocumentHistory (DocumentHistory (..))
import qualified Docs.Hasql.Statements as Statements
import qualified Docs.Hasql.Transactions as Transactions
import Docs.Hasql.TreeEdge (TreeEdgeChild (..))
import Docs.TextElement
    ( TextElement
    , TextElementID
    , TextElementKind
    )
import Docs.TextRevision
    ( NewTextRevision
    , TextElementRevision
    , TextRevision
    , TextRevisionConflict
    , TextRevisionHistory (TextRevisionHistory)
    , TextRevisionSelector
    )
import Docs.Tree (Edge (..), Node (..), NodeHeader)
import qualified Docs.Tree as Tree
import Docs.TreeRevision
    ( TreeRevision
    , TreeRevisionHistory (TreeRevisionHistory)
    , TreeRevisionSelector
    )
import Docs.Util (UserID)
import DocumentManagement.Hash (Hash)
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
    :: TextElementID
    -> TextRevisionSelector
    -> Session (Maybe TextElementRevision)
getTextElementRevision = curry $ flip statement Statements.getTextElementRevision

createTreeRevision
    :: UserID
    -> DocumentID
    -> Node TextElementID
    -> Session (TreeRevision TextElementID)
createTreeRevision authorID docID rootNode =
    transaction
        Serializable
        Write
        $ Transactions.createTreeRevision authorID docID rootNode

getTreeRevision
    :: DocumentID
    -> TreeRevisionSelector
    -> Session (TreeRevision TextElement)
getTreeRevision docID selector = do
    (rootHash, treeRevision) <- getRevision
    root <- getTree rootHash
    return $ treeRevision root
  where
    getRevision = statement (docID, selector) Statements.getTreeRevision

getTree :: Hash -> Session (Node TextElement)
getTree rootHash = do
    rootHeader <- statement rootHash Statements.getTreeNode
    fromHeader rootHash rootHeader
  where
    fromHeader :: Hash -> NodeHeader -> Session (Node TextElement)
    fromHeader hash header = do
        children <- statement hash Statements.getTreeEdgesByParent
        edges <- mapM edgeSelector children
        return $ Node header $ Vector.toList edges
    edgeSelector :: (Text, TreeEdgeChild) -> Session (Edge TextElement)
    edgeSelector (title, edge) =
        Edge title <$> case edge of
            (TreeEdgeToTextElement textElement) -> return $ Tree.Leaf textElement
            (TreeEdgeToNode hash header) -> fromHeader hash header <&> Tree.Tree

getTextRevisionHistory
    :: TextElementID -> Maybe UTCTime -> Session TextRevisionHistory
getTextRevisionHistory id_ before =
    statement (id_, before) Statements.getTextRevisionHistory
        <&> TextRevisionHistory id_ . Vector.toList

getTreeRevisionHistory
    :: DocumentID -> Maybe UTCTime -> Session TreeRevisionHistory
getTreeRevisionHistory id_ before =
    statement (id_, before) Statements.getTreeRevisionHistory
        <&> TreeRevisionHistory id_ . Vector.toList

getDocumentRevisionHistory
    :: DocumentID -> Maybe UTCTime -> Session DocumentHistory
getDocumentRevisionHistory id_ before =
    statement (id_, before) Statements.getDocumentRevisionHistory
        <&> DocumentHistory id_ . Vector.toList
