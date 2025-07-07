-- | Module for handling JSON data in the application.
module FPO.Data.JSON where

import Prelude

import Data.Argonaut (Json, decodeJson, encodeJson, (.:))
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Either (Either)
import FPO.Data.Store (Document, DocumentPlusPermission)

decodeDocument :: Json -> Either JsonDecodeError Document
decodeDocument json = do
  obj <- decodeJson json
  g <- obj .: "group"
  h <- obj .: "headCommit"
  i <- obj .: "id"
  n <- obj .: "name"
  pure { group: g, headCommit: h, id: i, name: n }

decodeDocumentWithPermission :: Json -> Either JsonDecodeError DocumentPlusPermission
decodeDocumentWithPermission json = do
  obj <- decodeJson json
  doc <- obj .: "document"
  docPerm <- obj .: "documentPermission"
  pure { document: doc, documentPermission: docPerm }

