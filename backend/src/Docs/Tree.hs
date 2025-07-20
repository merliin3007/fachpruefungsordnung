module Docs.Tree
    ( Tree (..)
    , Edge (..)
    , withTextRevisions
    , withMaybeTextRevisions
    ) where

import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Docs.TextElement (TextElement, TextElementID)
import qualified Docs.TextElement as TextElement
import Docs.TextRevision
    ( TextElementRevision (TextElementRevision)
    , TextRevision
    )

data Tree a
    = Node Text [Edge a]
    | Leaf a

data Edge a = Edge Text (Tree a)

instance Functor Tree where
    fmap f (Node header edge) = Node header $ (f <$>) <$> edge
    fmap f (Leaf x) = Leaf $ f x

instance Functor Edge where
    fmap f (Edge label tree) = Edge label $ f <$> tree

-- | Takes a tree and emplaces concrete text revision.
-- | The text revions are obtained via the specified getter function.
withTextRevisions
    :: (Monad m)
    => (TextElementID -> m TextRevision)
    -- ^ (potentially effectful) function for obtaining a text revision
    -> Tree TextElement
    -- ^ document structure tree
    -> m (Tree TextElementRevision)
    -- ^ document structure tree with concrete text revisions
withTextRevisions getTextRevision = withTextRevisions'
  where
    withTextRevisions' (Leaf textElement) =
        getTextRevision (TextElement.identifier textElement)
            <&> Leaf . TextElementRevision textElement
    withTextRevisions' (Node metaData edges) =
        mapM mapEdge edges <&> Node metaData
      where
        mapEdge (Edge label tree) =
            withTextRevisions' tree <&> Edge label

-- | Takes a tree and emplaces concrete text revisions.
-- | The text revisions are obtained via the specified getter function.
-- | This function may return 'Nothing'. In such a case, the corresponding Leaf is
-- | missing in the resulting tree revision.
-- | If the root is a leaf and is missing, the result will be 'Nothing'.
withMaybeTextRevisions
    :: (Monad m)
    => (TextElementID -> m (Maybe TextRevision))
    -- ^ (potentially effectful) function for obtaining a text revision
    -> Tree TextElement
    -- ^ document structure tree
    -> m (Maybe (Tree TextElementRevision))
    -- ^ document structure tree with concrete text revision
withMaybeTextRevisions getTextRevision = withMaybeTextRevisions'
  where
    withMaybeTextRevisions' (Leaf textElement) =
        getTextRevision (TextElement.identifier textElement)
            <&> (Leaf . TextElementRevision textElement <$>)
    withMaybeTextRevisions' (Node metaData edges) =
        mapM mapEdge edges <&> Just . Node metaData . catMaybes
      where
        mapEdge (Edge label tree) =
            withMaybeTextRevisions' tree <&> (Edge label <$>)
