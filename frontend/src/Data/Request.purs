-- | This module provides functions for making HTTP requests, including GET and POST requests.
-- | It supports various response formats such as String, JSON, Document, and Blob.
-- | The functions use the Affjax library to handle asynchronous HTTP requests.

module Data.Request where

import Prelude
import Affjax.Web (get, post) as AX
import Affjax.ResponseFormat (blob, document, json, string) as AXRF
import Effect.Aff (Aff)
import Data.Either (Either)
import Affjax (Error, Response)
import Data.Argonaut.Core (Json)
import Web.DOM.Document (Document)
import Web.File.Blob (Blob)
import Web.XHR.FormData (FormData)
import Affjax.RequestBody (document, json) as RequestBody
import Data.Maybe (Maybe(..))

-- | Makes a GET request to the given path and expects a String response.
getString :: String -> Aff (Either Error (Response String))
getString path = AX.get AXRF.string ("api" <> path)

-- | Makes a POST request to the given path with a JSON body and expects a String response.
postString :: String -> Json -> Aff (Either Error (Response String))
postString path body = AX.post AXRF.string ("/api" <> path) (Just $ RequestBody.json body)

-- | Makes a GET request to the given path and expects a JSON response.
getJson :: String -> Aff (Either Error (Response Json))
getJson path = AX.get AXRF.json ("/api" <> path)

-- | Makes a POST request to the given path with a JSON body and expects a JSON response.
postJson :: String -> Json -> Aff (Either Error (Response Json))
postJson path body = AX.post AXRF.json ("/api" <> path) (Just $ RequestBody.json body)

-- | Makes a GET request to the given path and expects a Document response.
getDocument :: String -> Aff (Either Error (Response Document))
getDocument path = AX.get AXRF.document ("/api" <> path)

-- | Makes a POST request to the given path with a JSON body and expects a Document response.
postDocument :: String -> Json -> Aff (Either Error (Response Document))
postDocument path body = AX.post AXRF.document ("/api" <> path) (Just $ RequestBody.json body)

-- | Makes a GET request to the given path and expects a Blob response.
getBlob :: String -> Aff (Either Error (Response Blob))
getBlob path = AX.get AXRF.blob ("/api" <> path)

-- | Makes a POST request to the given path with a JSON body and expects a Blob response.
postBlob :: String -> Json -> Aff (Either Error (Response Blob))
postBlob path body = AX.post AXRF.blob ("/api" <> path) (Just $ RequestBody.json body)
