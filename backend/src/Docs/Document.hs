{-# LANGUAGE DeriveGeneric #-}

module Docs.Document
    ( DocumentID (..)
    , Document (..)
    ) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time (UTCTime)

import GHC.Generics (Generic)
import GHC.Int (Int32)

import Web.HttpApiData (FromHttpApiData (..))

import Control.Lens ((&), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.OpenApi
    ( OpenApiType (..)
    , ToParamSchema (..)
    , ToSchema (..)
    , exclusiveMinimum
    , minimum_
    , type_
    )

import UserManagement.Group (GroupID)
import UserManagement.User (UserID)

-- | ID for a document
newtype DocumentID = DocumentID
    { unDocumentID :: Int32
    }
    deriving (Eq, Show)

instance ToJSON DocumentID where
    toJSON = toJSON . unDocumentID

instance FromJSON DocumentID where
    parseJSON = fmap DocumentID . parseJSON

instance ToSchema DocumentID where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int32)

instance ToParamSchema DocumentID where
    toParamSchema _ =
        mempty
            & type_ ?~ OpenApiInteger
            & minimum_ ?~ 0
            & exclusiveMinimum ?~ False

instance FromHttpApiData DocumentID where
    parseUrlPiece = (DocumentID <$>) . parseUrlPiece

-- | Document metadata
data Document = Document
    { identifier :: DocumentID
    , name :: Text
    , group :: GroupID
    , lastEdited :: Maybe UTCTime
    , lastEditedBy :: Maybe UserID
    }
    deriving (Eq, Show, Generic)

instance ToJSON Document

instance FromJSON Document

instance ToSchema Document
