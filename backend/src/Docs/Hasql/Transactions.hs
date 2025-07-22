{-# LANGUAGE TupleSections #-}

module Docs.Hasql.Transactions
    ( createTextRevision
    , putTree
    ) where

import qualified Crypto.Hash.SHA1 as SHA1
import Hasql.Transaction (Transaction, statement)

import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Docs.Hasql.Statements as Statements
import Docs.Hasql.TreeEdge (TreeEdge (TreeEdge), TreeEdgeChildRef (..))
import qualified Docs.Hasql.TreeEdge as TreeEdge
import Docs.TextElement (TextElementID)
import Docs.TextRevision
    ( NewTextRevision
    , TextRevision
    , TextRevisionConflict
    , newTextRevision
    )
import Docs.Tree (Edge (..), Node (..), Tree (..))
import Docs.Util (UserID)
import DocumentManagement.Hash
    ( Hash (Hash)
    , Hashable (..)
    )

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

putTree :: Node TextElementID -> Transaction Hash
putTree (Node metaData children) = do
    childRefs <- mapM putChild children
    let ownHash =
            Hash $
                SHA1.finalize
                    ( foldr
                        (flip updateHash . fst)
                        (updateHash SHA1.init metaData)
                        childRefs
                    )
    statement (ownHash, metaData) Statements.putTreeNode
    let toEdge (ref, label) idx =
            TreeEdge
                { TreeEdge.parentHash = ownHash
                , TreeEdge.position = idx
                , TreeEdge.title = label
                , TreeEdge.child = ref
                }
    let edges = zipWith toEdge childRefs [0 ..]
    mapM_ (`statement` Statements.putTreeEdge) edges
    return ownHash
  where
    putChild :: Edge TextElementID -> Transaction (TreeEdgeChildRef, Text)
    putChild (Edge label node) = putSubTree node <&> (,label)
    putSubTree :: Tree TextElementID -> Transaction TreeEdgeChildRef
    putSubTree (Leaf id_) = return $ TreeEdgeToTextElement id_
    putSubTree (Tree node) = putTree node <&> TreeEdgeToNode
