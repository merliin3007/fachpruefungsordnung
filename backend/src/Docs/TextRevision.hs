{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Docs.TextRevision
    ( TextRevisionID (..)
    , TextRevisionSelector (..)
    , TextRevision (..)
    , TextRevisionHeader (..)
    , TextElementRevision (..)
    , TextRevisionConflict (..)
    , TextRevisionHistory (..)
    , NewTextRevision (..)
    , TextRevisionRef (..)
    , textRevisionRef
    , newTextRevision
    , specificTextRevision
    ) where

import Data.Functor ((<&>))
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.UUID (UUID)

import Text.Read (readMaybe)

import GHC.Generics (Generic)
import GHC.Int (Int32)

import Control.Lens ((&), (.~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi
    ( NamedSchema (..)
    , OpenApiType (..)
    , Referenced (Inline)
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
import Docs.TextElement
    ( TextElement
    , TextElementID
    , TextElementRef (..)
    )

data TextRevisionRef
    = TextRevisionRef
        TextElementRef
        TextRevisionSelector

textRevisionRef
    :: DocumentID
    -> TextElementID
    -> TextRevisionSelector
    -> TextRevisionRef
textRevisionRef = (TextRevisionRef .) . TextElementRef

-- | ID for a text revision
newtype TextRevisionID = TextRevisionID
    { unTextRevisionID :: Int32
    }
    deriving (Eq, Ord, Show)

instance ToJSON TextRevisionID where
    toJSON = toJSON . unTextRevisionID

instance FromJSON TextRevisionID where
    parseJSON = fmap TextRevisionID . parseJSON

instance ToSchema TextRevisionID where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int32)

instance ToParamSchema TextRevisionID where
    toParamSchema _ =
        mempty
            & type_ ?~ OpenApiInteger
            & minimum_ ?~ 0
            & exclusiveMinimum ?~ False

instance FromHttpApiData TextRevisionID where
    parseUrlPiece = (TextRevisionID <$>) . parseUrlPiece

-- | Selector for a text revision
data TextRevisionSelector
    = Latest
    | Specific TextRevisionID

instance ToJSON TextRevisionSelector where
    toJSON Latest = toJSON ("latest" :: Text)
    toJSON (Specific id_) = toJSON id_

instance FromJSON TextRevisionSelector where
    parseJSON v = case v of
        Aeson.String t ->
            if t == "latest"
                then pure Latest
                else fail $ "Invalid string for TextRevisionSelector: " ++ Text.unpack t
        Aeson.Number n -> case toBoundedInteger n of
            Just i -> pure $ Specific $ TextRevisionID i
            Nothing -> fail "Invalid number for Int32"
        _ -> fail "TextRevisionSelector must be either a string \"latest\" or an integer"

instance ToSchema TextRevisionSelector where
    declareNamedSchema _ = do
        intSchema <- declareSchemaRef (Proxy :: Proxy Int32)
        let latestSchema =
                Inline $
                    mempty
                        & type_ ?~ OpenApiString
                        & enum_ ?~ ["latest"]
        return $
            NamedSchema (Just "TextRevisionSelector") $
                mempty & oneOf ?~ [latestSchema, intSchema]

instance ToParamSchema TextRevisionSelector where
    toParamSchema _ =
        mempty
            & oneOf
                ?~ [ Inline $
                        mempty
                            & type_ ?~ OpenApiString
                            & enum_ ?~ ["latest"]
                   , Inline $
                        mempty
                            & type_ ?~ OpenApiInteger
                            & minimum_ ?~ 0
                            & exclusiveMinimum ?~ False
                   ]

instance FromHttpApiData TextRevisionSelector where
    parseUrlPiece txt
        | Text.toLower txt == "latest" = Right Latest
        | otherwise = case readMaybe (Text.unpack txt) of
            Just i -> Right $ Specific $ TextRevisionID i
            Nothing -> Left $ "Invalid TextRevisionSelector: " <> txt

specificTextRevision :: TextRevisionSelector -> Maybe TextRevisionID
specificTextRevision Latest = Nothing
specificTextRevision (Specific id_) = Just id_

data TextRevisionHeader = TextRevisionHeader
    { identifier :: TextRevisionID
    , timestamp :: UTCTime
    , author :: UUID
    }
    deriving (Show, Generic)

instance ToJSON TextRevisionHeader

instance FromJSON TextRevisionHeader

instance ToSchema TextRevisionHeader

data TextRevision
    = TextRevision
        TextRevisionHeader
        Text

data TextElementRevision
    = TextElementRevision
        TextElement
        (Maybe TextRevision)

data TextRevisionHistory
    = TextRevisionHistory
        TextElementRef
        [TextRevisionHeader]

data NewTextRevision = NewTextRevision
    { newTextRevisionElement :: TextElementRef
    , newTextRevisionParent :: Maybe TextRevisionID
    , newTextRevisionContent :: Text
    }

newtype TextRevisionConflict
    = TextRevisionConflict TextRevisionID -- todo: maybe not id but whole TextRevision?

-- | Creates a new text revision.
-- | Returns a conflict, if the parent revision is not the latest revision.
-- | If the text element does not have any revision, but a parent revision is set,
-- | it is ignored.
newTextRevision
    :: (Monad m)
    => (TextElementRef -> m (Maybe TextRevisionID))
    -- ^ gets the latest revision id for a text element (if any)
    -> (UserID -> TextElementRef -> Text -> m TextRevision)
    -- ^ creates a new text revision in the database
    -> UserID
    -- ^ the id of the user who intends to create the new revision
    -> NewTextRevision
    -- ^ all data needed to create a new text revision
    -> m (Either TextRevisionConflict TextRevision)
    -- ^ either the newly created text revision or a conflict
newTextRevision getLatestRevisionID createRevision userID newRevision = do
    latestRevisionID <- getLatestRevisionID $ newTextRevisionElement newRevision
    let parentRevisionID = newTextRevisionParent newRevision
    case latestRevisionID of
        Nothing -> createRevision' <&> Right
        Just latest
            | latestRevisionID == parentRevisionID -> createRevision' <&> Right
            | otherwise -> return $ Left $ TextRevisionConflict latest
  where
    createRevision' =
        createRevision
            userID
            (newTextRevisionElement newRevision)
            (newTextRevisionContent newRevision)
