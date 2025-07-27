{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Docs.TreeRevision
    ( TreeRevisionID (..)
    , TreeRevision (..)
    , TreeRevisionHeader (..)
    , TreeRevisionSelector (..)
    , TreeRevisionHistory (..)
    , TreeRevisionRef (..)
    , mapRoot
    , mapMRoot
    , replaceRoot
    , withTextRevisions
    , newTreeRevision
    , specificTreeRevision
    ) where

import Control.Monad (unless)
import Data.Functor ((<&>))
import Data.Proxy (Proxy (Proxy))
import Data.Time (UTCTime)
import Data.UUID (UUID)

import GHC.Generics (Generic)
import GHC.Int (Int32)

import Control.Lens ((&), (.~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi
    ( NamedSchema (..)
    , OpenApiType (..)
    , Referenced (Inline)
    , Schema (..)
    , ToParamSchema (..)
    , ToSchema (..)
    , declareSchemaRef
    , enum_
    , exclusiveMinimum
    , minimum_
    , oneOf
    , properties
    , required
    , type_
    )
import Web.HttpApiData (FromHttpApiData (..))

import UserManagement.User (UserID)

import Docs.Document (DocumentID)
import Docs.TextElement (TextElement, TextElementID)
import Docs.TextRevision (TextElementRevision, TextRevision)
import Docs.Tree (Node)
import qualified Docs.Tree as Tree

data TreeRevisionRef
    = TreeRevisionRef
        DocumentID
        TreeRevisionSelector

data TreeRevisionSelector
    = Latest
    | Specific TreeRevisionID

specificTreeRevision :: TreeRevisionSelector -> Maybe TreeRevisionID
specificTreeRevision Latest = Nothing
specificTreeRevision (Specific id_) = Just id_

-- | An ID for a tree revision.
newtype TreeRevisionID = TreeRevisionID
    { unTreeRevisionID :: Int32
    }
    deriving (Eq)

instance ToJSON TreeRevisionID where
    toJSON = toJSON . unTreeRevisionID

instance FromJSON TreeRevisionID where
    parseJSON = fmap TreeRevisionID . parseJSON

instance ToSchema TreeRevisionID where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int32)

instance ToParamSchema TreeRevisionID where
    toParamSchema _ =
        mempty
            & type_ ?~ OpenApiInteger
            & minimum_ ?~ 0
            & exclusiveMinimum ?~ False

instance FromHttpApiData TreeRevisionID where
    parseUrlPiece = (TreeRevisionID <$>) . parseUrlPiece

-- | Contains metadata about a tree revision.
data TreeRevisionHeader = TreeRevisionHeader
    { identifier :: TreeRevisionID
    , timestamp :: UTCTime
    , author :: UUID
    }
    deriving (Generic)

instance ToJSON TreeRevisionHeader

instance FromJSON TreeRevisionHeader

instance ToSchema TreeRevisionHeader

-- | A tree revision.
data TreeRevision a
    = TreeRevision
        TreeRevisionHeader
        (Node a)

instance (ToJSON a) => ToJSON (TreeRevision a) where
    toJSON (TreeRevision header root) =
        Aeson.object ["header" .= header, "root" .= root]

instance (FromJSON a) => FromJSON (TreeRevision a) where
    parseJSON = Aeson.withObject "TreeRevision" $ \v ->
        TreeRevision
            <$> v .: "header"
            <*> v .: "root"

data TreeRevisionHistory
    = TreeRevisionHistory
        DocumentID
        [TreeRevisionHeader]

mapRoot :: (Node a -> Node b) -> TreeRevision a -> TreeRevision b
mapRoot f (TreeRevision header root) = TreeRevision header $ f root

replaceRoot :: Node b -> TreeRevision a -> TreeRevision b
replaceRoot new = mapRoot $ const new

mapMRoot
    :: (Monad m)
    => (Node a -> m (Node b))
    -> TreeRevision a
    -> m (TreeRevision b)
mapMRoot f (TreeRevision header root) = f root <&> TreeRevision header

instance Functor TreeRevision where
    fmap f (TreeRevision header root) = TreeRevision header $ f <$> root

-- | Takes a tree revision and emplaces concrecte text revisions.
-- | The text revisions are obtained via the specified getter function.
withTextRevisions
    :: (Monad m)
    => (TextElementID -> m (Maybe TextRevision))
    -- ^ (potentially effectful) function for obtaining a text revision
    -> TreeRevision TextElement
    -- ^ document structre tree revision
    -> m (TreeRevision TextElementRevision)
    -- ^ document structre tree revision with concrete text revision
withTextRevisions getTextRevision = mapMRoot (Tree.withTextRevisions getTextRevision)

newTreeRevision
    :: (Monad m)
    => (DocumentID -> m (TextElementID -> Bool))
    -- ^ for a given document, checks if a text element belongs to this document
    -> (UserID -> DocumentID -> Node TextElementID -> m (TreeRevision TextElementID))
    -- ^ create a new tree revision
    -> UserID
    -- ^ authors user id
    -> DocumentID
    -- ^ which document the revision belongs to
    -> Node TextElementID
    -- ^ the root node of the revision
    -> m (TreeRevision TextElementID)
    -- ^ newly created revision
newTreeRevision
    isTextElementInDocument
    createRevision
    authorID
    docID
    rootNode = do
        isTextElementInDocument' <- isTextElementInDocument docID
        let allTextElementsBelongToDocument =
                all isTextElementInDocument' rootNode
        unless allTextElementsBelongToDocument $
            error "Not all referenced text elements belong to the document."
        createRevision authorID docID rootNode
