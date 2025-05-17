module VersionControl.Sessions
    ( createCommit
    , getCommit
    , getVersion
    )
where

import Data.Text
import Data.Vector (toList)
import Hasql.Session (Session, statement)
import Hasql.Transaction.Sessions
    ( IsolationLevel (..)
    , Mode (..)
    , transaction
    )
import VersionControl.Commit
import VersionControl.Hash
import qualified VersionControl.Statements as Statements
import qualified VersionControl.Transactions as Transactions
import VersionControl.Tree

getCommit :: CommitID -> Session ExistingCommit
getCommit id' = do
    commit <- statement id' Statements.getCommit
    replaceRoot commit
  where
    replaceRoot (ExistingCommit header (CommitBody info (Ref ref))) = do
        valueRoot <- getVersion ref
        return $ ExistingCommit header $ CommitBody info $ Value valueRoot
    replaceRoot commit = return commit

createCommit :: CreateCommit -> Session ExistingCommit
createCommit commit =
    transaction
        Serializable
        Write
        $ Transactions.createCommit commit

getVersion :: Hash -> Session (Tree (Hashed NodeWithRef))
getVersion hs = do
    root <- statement hs Statements.getVersion
    withChildren root
  where
    withChildren
        :: Hashed NodeWithRef
        -> Session (Tree (Hashed NodeWithRef))
    withChildren parent@(Hashed hs' _) = do
        children <- statement hs' Statements.getChildrenByParentHash
        edges <- mapM toEdge $ toList children
        return $ Tree parent edges
    toEdge
        :: (Text, Hashed NodeWithRef)
        -> Session (Edge (Hashed NodeWithRef))
    toEdge (title, node) = do
        tree <- withChildren node
        return $ Edge title $ Value tree
