module Docs.Hasql.Transactions
    ( createTextRevision
    ) where

import Hasql.Transaction (Transaction, statement)

import qualified Docs.Hasql.Statements as Statements
import Docs.Hasql.TreeEdge (TreeEdge (TreeEdge))
import qualified Docs.Hasql.TreeEdge as TreeEdge
import Docs.TextElement (TextElementID)
import Docs.TextRevision
    ( NewTextRevision
    , TextRevision
    , TextRevisionConflict
    , newTextRevision
    )
import Docs.Tree (Tree (..))
import Docs.Util (UserID)
import DocumentManagement.Hash (Hashed)

createTextRevision
    :: UserID
    -- ^ the id of the user who intends to create the new revision
    -> NewTextRevision
    -- ^ all data needed to create a new text revision
    -> Transaction (Either TextRevisionConflict TextRevision)
    -- ^ either the newly created text revision or a conflict
createTextRevision =
    newTextRevision
        (`statement` Statements.getLatestTextRevisionID)
        ( \textElementID userID content ->
            statement
                (textElementID, userID, content)
                Statements.createTextRevision
        )

putTree :: Tree TextElementID -> Transaction (Hashed (Tree TextElementID))
putTree = undefined
  where
    putEdge :: TreeEdge -> Transaction ()
    putEdge = (`statement` Statements.putTreeEdge)
