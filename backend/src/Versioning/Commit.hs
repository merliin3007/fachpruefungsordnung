{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Versioning.Commit
    ( CreateCommit (..)
    , ExistingCommit (..)
    , CommitHeader (..)
    , CommitInfo (..)
    , CommitBody (..)
    , CommitID (..)
    , CommitRef
    , commitRefID
    , commitIDInt32
    , commitMapRoot
    )
where

import Control.Lens ((&), (.~), (?~))
import Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi
    ( NamedSchema (..)
    , OpenApiType (..)
    , ToSchema (..)
    , declareSchemaRef
    , properties
    , required
    , type_
    )
import Data.Proxy
import Data.Text
import Data.Time (LocalTime)
import Data.UUID
import GHC.Generics
import GHC.Int
import Versioning.Hash
import Versioning.Tree

-- the id of a commit
newtype CommitID = CommitID Int32 deriving (Show, Generic)

instance ToJSON CommitID

instance ToSchema CommitID

commitIDInt32 :: CommitID -> Int32
commitIDInt32 (CommitID i) = i

-- a reference to a commit or the commit itself
type CommitRef = Ref CommitID ExistingCommit

commitRefID :: CommitRef -> CommitID
commitRefID (Ref ref) = ref
commitRefID (Value (ExistingCommit header _)) = commitID header

-- contains all information needed to create a new commit
data CreateCommit
    = CreateCommit
        CommitInfo
        (TreeRef NodeWithMaybeRef)
    deriving (Show)

instance ToJSON CreateCommit where
    toJSON (CreateCommit info root) =
        Aeson.object ["info" .= info, "root" .= root]

-- a commit guaranteed to exist in the database
data ExistingCommit
    = ExistingCommit
        CommitHeader
        CommitBody
    deriving (Show)

instance ToJSON ExistingCommit where
    toJSON (ExistingCommit header body) =
        Aeson.object ["header" .= header, "body" .= body]

instance ToSchema ExistingCommit where
    declareNamedSchema _ = do
        headerSchema <- declareSchemaRef (Proxy :: Proxy CommitHeader)
        bodySchema <- declareSchemaRef (Proxy :: Proxy CommitBody)
        return $
            NamedSchema (Just "ExistingCommit") $
                mempty
                    & type_
                        ?~ OpenApiObject
                    & properties
                        .~ InsOrd.fromList
                            [ ("header", headerSchema)
                            , ("body", bodySchema)
                            ]
                    & required
                        .~ ["header", "body"]

commitMapRoot
    :: (TreeRef (Hashed NodeWithRef) -> TreeRef (Hashed NodeWithRef))
    -> ExistingCommit
    -> ExistingCommit
commitMapRoot f (ExistingCommit header (CommitBody info root)) =
    ExistingCommit header (CommitBody info (f root))

-- contains the content of an existing commit
data CommitBody
    = CommitBody
        CommitInfo
        (TreeRef (Hashed NodeWithRef))
    deriving (Show)

instance ToJSON CommitBody where
    toJSON (CommitBody info root) =
        Aeson.object ["info" .= info, "root" .= root]

instance ToSchema CommitBody where
    declareNamedSchema _ = do
        infoSchema <- declareSchemaRef (Proxy :: Proxy CommitInfo)
        rootSchema <- declareSchemaRef (Proxy :: Proxy Text) -- Todo
        return $
            NamedSchema (Just "CommitBody") $
                mempty
                    & type_
                        ?~ OpenApiObject
                    & properties
                        .~ InsOrd.fromList
                            [ ("info", infoSchema)
                            , ("root", rootSchema)
                            ]
                    & required
                        .~ ["info", "root"]

-- additional metadata for existing commits
data CommitHeader = CommitHeader
    { commitID :: CommitID
    , creationTs :: LocalTime
    }
    deriving (Show, Generic)

instance ToJSON CommitHeader

instance ToSchema CommitHeader

-- metadata about a commit
data CommitInfo = CommitInfo
    { commitAuthor :: UUID
    , commitMessage :: Maybe Text
    , parentCommit :: Maybe CommitRef
    }
    deriving (Show, Generic)

instance ToJSON CommitInfo

instance ToSchema CommitInfo
