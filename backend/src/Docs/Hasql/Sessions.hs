module Docs.Hasql.Sessions
    ( createDocument
    , getDocument
    , createTextElement
    , createTextRevision
    , getTextElementRevision
    , createTreeRevision
    , getTreeRevision
    , getTree
    ) where

import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Vector as Vector

import Hasql.Session (Session, statement)
import Hasql.Transaction.Sessions
    ( IsolationLevel (..)
    , Mode (..)
    , transaction
    )

import Docs.Document (Document, DocumentID)
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
    , TextRevisionSelector
    )
import qualified Docs.TextRevision as TextRevision
import Docs.Tree (Edge (..), Node (..), NodeHeader)
import qualified Docs.Tree as Tree
import Docs.TreeRevision (TreeRevision, TreeRevisionSelector)
import qualified Docs.TreeRevision as TreeRevision
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
    :: TextRevisionSelector
    -> Session (Maybe TextElementRevision)
getTextElementRevision (TextRevision.Latest textElementID) =
    statement textElementID Statements.getLatestTextElementRevision
getTextElementRevision (TextRevision.Specific textRevisionID) =
    statement textRevisionID Statements.getTextElementRevision

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
    :: TreeRevisionSelector -> Session (TreeRevision TextElement)
getTreeRevision selector = do
    (rootHash, treeRevision) <- getRevision
    root <- getTree rootHash
    return $ treeRevision root
  where
    getRevision = case selector of
        (TreeRevision.Latest docID) ->
            statement docID Statements.getLatestTreeRevision
        (TreeRevision.Specific revID) ->
            statement revID Statements.getTreeRevision

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
