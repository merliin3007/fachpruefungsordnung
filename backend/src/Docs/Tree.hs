module Docs.Tree
    ( Tree (..)
    , Edge (..)
    , Node (..)
    , withTextRevisions
    ) where

import Data.Functor ((<&>))
import Data.Text (Text)

import Docs.TextElement (TextElement, TextElementID)
import qualified Docs.TextElement as TextElement
import Docs.TextRevision
    ( TextElementRevision (TextElementRevision)
    , TextRevision
    )

data Node a = Node Text [Edge a]

data Tree a
    = Tree (Node a)
    | Leaf a

data Edge a = Edge Text (Tree a)

instance Functor Node where
    fmap f (Node header edge) = Node header $ (f <$>) <$> edge

instance Functor Tree where
    fmap f (Tree node) = Tree $ f <$> node
    fmap f (Leaf x) = Leaf $ f x

instance Functor Edge where
    fmap f (Edge label tree) = Edge label $ f <$> tree

-- | Takes a tree and emplaces concrete text revision.
-- | The text revions are obtained via the specified getter function.
withTextRevisions
    :: (Monad m)
    => (TextElementID -> m (Maybe TextRevision))
    -- ^ (potentially effectful) function for obtaining a text revision
    -> Node TextElement
    -- ^ document structure tree
    -> m (Node TextElementRevision)
    -- ^ document structure tree with concrete text revisions
withTextRevisions getTextRevision = withTextRevisions'
  where
    withTextRevisions' (Node metaData edges) = mapM mapEdge edges <&> Node metaData
    treeWithTextRevisions (Tree node) = withTextRevisions' node <&> Tree
    treeWithTextRevisions (Leaf textElement) =
        getTextRevision (TextElement.identifier textElement)
            <&> Leaf . TextElementRevision textElement
    mapEdge (Edge label tree) = treeWithTextRevisions tree <&> Edge label
