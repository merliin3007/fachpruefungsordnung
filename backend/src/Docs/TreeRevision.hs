module Docs.TreeRevision
    ( TreeRevisionID (..)
    , TreeRevision (..)
    , ExistingTreeRevision (..)
    , mapRoot
    , mapMRoot
    , replaceRoot
    ) where

import Data.Time (UTCTime)
import Data.UUID (UUID)
import Docs.Tree (Tree)
import GHC.Int (Int32)

newtype TreeRevisionID = TreeRevisionID
    { unTreeRevisionID :: Int32
    }

data TreeRevision a = TreeRevision
    { timestamp :: UTCTime
    , author :: UUID
    , root :: Tree a
    }

mapRoot :: (Tree a -> Tree b) -> TreeRevision a -> TreeRevision b
mapRoot f treeVersion =
    TreeRevision
        { timestamp = timestamp treeVersion
        , author = author treeVersion
        , root = f $ root treeVersion
        }

replaceRoot :: Tree b -> TreeRevision a -> TreeRevision b
replaceRoot new = mapRoot $ const new

mapMRoot
    :: (Monad m)
    => (Tree a -> m (Tree b))
    -> TreeRevision a
    -> m (TreeRevision b)
mapMRoot f treeVersion = do
    new <- f $ root treeVersion
    return $ replaceRoot new treeVersion

data ExistingTreeRevision a
    = ExistingTreeRevision
        TreeRevisionID
        (TreeRevision a)

instance Functor TreeRevision where
    fmap f treeVersion =
        TreeRevision
            { timestamp = timestamp treeVersion
            , author = author treeVersion
            , root = f <$> root treeVersion
            }

instance Functor ExistingTreeRevision where
    fmap f (ExistingTreeRevision id_ treeVersion) =
        ExistingTreeRevision id_ $ f <$> treeVersion
