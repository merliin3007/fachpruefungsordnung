module Docs.Tree
    ( Tree (..)
    , Edge (..)
    , TreeVersionID (..)
    , TreeVersion (..)
    , ExistingTreeVersion (..)
    , TreeVersionToC
    , TreeVersionFull
    ) where

import Data.Text (Text)
import Data.Time (LocalTime)
import Data.UUID (UUID)
import Docs.Text (TextElement, TextElementVersion)
import GHC.Int (Int32)

data Tree a
    = Node Text [Edge a]
    | Leaf a

data Edge a = Edge Text (Tree a)

newtype TreeVersionID = StructureVersionID
    { unTreeVersionID :: Int32
    }

data TreeVersion a = TreeVersion
    { treeVersionTimestamp :: LocalTime
    , treeVersionAuthor :: UUID
    , treeVersionRoot :: Tree a
    }

data ExistingTreeVersion a
    = ExistingTreeVersion
        TreeVersionID
        (TreeVersion a)

type TreeVersionToC = ExistingTreeVersion TextElement

type TreeVersionFull = ExistingTreeVersion TextElementVersion

instance Functor Tree where
    fmap f (Node header edge) = Node header $ (f <$>) <$> edge
    fmap f (Leaf x) = Leaf $ f x

instance Functor Edge where
    fmap f (Edge label tree) = Edge label $ f <$> tree

instance Functor TreeVersion where
    fmap f treeVersion =
        TreeVersion
            { treeVersionTimestamp = treeVersionTimestamp treeVersion
            , treeVersionAuthor = treeVersionAuthor treeVersion
            , treeVersionRoot = f <$> treeVersionRoot treeVersion
            }

instance Functor ExistingTreeVersion where
    fmap f (ExistingTreeVersion id_ treeVersion) =
        ExistingTreeVersion id_ $ f <$> treeVersion
