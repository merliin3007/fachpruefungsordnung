-- | This module provides functions for making HTTP requests, including GET and POST requests.
-- | It supports various response formats such as String, JSON, Document, and Blob.
-- | The functions use the Affjax library to handle asynchronous HTTP requests.

module FPO.Data.Request where

import Prelude

import Affjax (AffjaxDriver, Error, Request, Response, request)
import Affjax.RequestBody (json) as RequestBody
import Affjax.RequestHeader (RequestHeader(RequestHeader))
import Affjax.ResponseFormat (ResponseFormat)
import Affjax.ResponseFormat (blob, document, ignore, json, string) as AXRF
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut (JsonDecodeError, decodeJson, encodeJson, fromString)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Decoders (decodeArray)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import FPO.Data.AppError (AppError(..), handleAppError, printAjaxError)
import FPO.Data.Navigate (class Navigate)
import FPO.Dto.CreateDocumentDto (NewDocumentCreateDto)
import FPO.Dto.DocumentDto.DocumentHeader as DH
import FPO.Dto.DocumentDto.Query as DQ
import FPO.Dto.GroupDto
  ( GroupCreate
  , GroupDto
  , GroupID
  , GroupOverview
  , toGroupOverview
  )
import FPO.Dto.UserDto
  ( FullUserDto
  , UserID
  , getAllAdminRoles
  , isAdminOf
  , isUserSuperadmin
  )
import FPO.Dto.UserRoleDto (Role)
import Halogen as H
import Web.DOM.Document (Document)
import Web.File.Blob (Blob)

-- | Foreign imports
foreign import driver :: AffjaxDriver
foreign import getCookieEff :: String -> Effect String

-- | Helper-Funktion zum Erstellen einer FPO-Request mit XSRF-Token
defaultFpoRequest
  :: forall a. ResponseFormat a -> String -> Method -> Effect (Request a)
defaultFpoRequest responseFormat url method = do
  xsrfToken <- getCookieEff "XSRF-TOKEN"
  pure
    { method: Left method
    , url
    , headers: [ RequestHeader "X-XSRF-TOKEN" xsrfToken ]
    , content: Nothing
    , username: Nothing
    , password: Nothing
    , withCredentials: false
    , responseFormat
    , timeout: Nothing
    }

-- | High-level requests ---------------------------------------------------

-- | State of an asynchronous load operation.
-- | It can either be in a loading state or have successfully loaded data.
data LoadState a = Loading | Loaded a

-- | Generic Error Handling Functions ----------------------------------------

-- | Generic wrapper for any Aff request that returns Either Error (Response a)
-- | Converts Ajax errors and HTTP status codes to AppError
handleRequestWithError
  :: forall a st act slots msg m
   . MonadAff m
  => Navigate m
  => String -- URL for error context
  -> Aff (Either Error (Response a))
  -> H.HalogenM st act slots msg m (Either AppError a)
handleRequestWithError url requestAction = do
  response <- H.liftAff requestAction
  case response of
    Left err ->
      pure $ Left $ NetworkError $ printAjaxError "Connection failed" err
    Right { body, status } -> do
      case status of
        StatusCode 401 -> do
          handleAppError AuthError
          pure $ Left AuthError
        StatusCode 403 ->
          pure $ Left AccessDeniedError
        StatusCode 404 ->
          pure $ Left $ NotFoundError url
        StatusCode 405 ->
          pure $ Left $ MethodNotAllowedError url "Unknown"
        StatusCode code | code >= 500 && code < 600 ->
          pure $ Left $ ServerError $ "Server error (status: " <> show code <> ")"
        StatusCode 200 ->
          pure $ Right body
        StatusCode 201 ->
          pure $ Right body
        StatusCode 204 ->
          pure $ Right body
        StatusCode code ->
          pure $ Left $ ServerError $ "Unexpected status code: " <> show code

-- | Wrapper specifically for JSON responses with decode step
handleJsonRequestWithError
  :: forall a st act slots msg m
   . MonadAff m
  => Navigate m
  => (Json -> Either JsonDecodeError a)
  -> String
  -> Aff (Either Error (Response Json))
  -> H.HalogenM st act slots msg m (Either AppError a)
handleJsonRequestWithError decode url requestAction = do
  result <- handleRequestWithError url requestAction
  case result of
    Left appError -> pure $ Left appError
    Right json -> do
      case decode json of
        Left err -> do
          liftEffect $ log $ "JSON decode error: " <> show err
          pure $ Left $ DataError $ "Invalid data format: " <> show err
        Right val ->
          pure $ Right val

-- | Helper for requests that don't return meaningful body (Unit responses)
handleUnitRequestWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => String
  -> Aff (Either Error (Response Unit))
  -> H.HalogenM st act slots msg m (Either AppError Unit)
handleUnitRequestWithError = handleRequestWithError

getFromJSONEndpoint
  :: forall a. (Json -> Either JsonDecodeError a) -> String -> Aff (Maybe a)
getFromJSONEndpoint decode url = do
  response <- getJson url
  case response of
    Left _ ->
      pure Nothing
    Right res -> do
      case decode (res.body) of
        Left err -> do
          liftEffect $ log $ "Error Decoding: " <> show err
          pure Nothing
        Right val -> do
          pure $ Just val

-- | Simplified Error-Handling HTTP Methods ----------------------------------

-- | Error-handling versions of basic HTTP methods
getStringWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError String)
getStringWithError url = handleRequestWithError url (getString url)

getJsonWithError
  :: forall a st act slots msg m
   . MonadAff m
  => Navigate m
  => (Json -> Either JsonDecodeError a)
  -> String
  -> H.HalogenM st act slots msg m (Either AppError a)
getJsonWithError decode url = handleJsonRequestWithError decode url (getJson url)

getBlobWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError Blob)
getBlobWithError url = handleRequestWithError url (getBlob url)

getDocumentWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError Document)
getDocumentWithError url = handleRequestWithError url (getDocument url)

getIgnoreWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError Unit)
getIgnoreWithError url = handleUnitRequestWithError url (getIgnore url)

postJsonWithError
  :: forall a st act slots msg m
   . MonadAff m
  => Navigate m
  => (Json -> Either JsonDecodeError a)
  -> String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError a)
postJsonWithError decode url body = handleJsonRequestWithError decode url
  (postJson url body)

postStringWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError String)
postStringWithError url body = handleRequestWithError url (postString url body)

postBlobWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError Blob)
postBlobWithError url body = handleRequestWithError url (postBlob url body)

postDocumentWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError Document)
postDocumentWithError url body = handleRequestWithError url (postDocument url body)

putJsonWithError
  :: forall a st act slots msg m
   . MonadAff m
  => Navigate m
  => (Json -> Either JsonDecodeError a)
  -> String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError a)
putJsonWithError decode url body = handleJsonRequestWithError decode url
  (putJson url body)

putIgnoreWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError Unit)
putIgnoreWithError url body = handleUnitRequestWithError url (putIgnore url body)

patchJsonWithError
  :: forall a st act slots msg m
   . MonadAff m
  => Navigate m
  => (Json -> Either JsonDecodeError a)
  -> String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError a)
patchJsonWithError decode url body = handleJsonRequestWithError decode url
  (patchJson url body)

patchStringWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError String)
patchStringWithError url body = handleRequestWithError url (patchString url body)

deleteIgnoreWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError Unit)
deleteIgnoreWithError url = handleUnitRequestWithError url (deleteIgnore url)

-- | Special request functions
patchToStringEndpointWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => String
  -> String
  -> H.HalogenM st act slots msg m (Either AppError String)
patchToStringEndpointWithError url requestBody = do
  response <- H.liftAff $ patchString url (fromString requestBody)
  case response of
    Left err ->
      -- Network/connection errors
      pure $ Left $ NetworkError $ printAjaxError "Connection failed" err
    Right { body, status } -> do
      case status of
        StatusCode 401 -> do
          handleAppError AuthError
          pure $ Left AuthError
        StatusCode 403 -> pure $ Left AccessDeniedError
        StatusCode 404 -> pure $ Left $ NotFoundError url
        StatusCode 405 -> pure $ Left $ MethodNotAllowedError url "PATCH"
        StatusCode code | code >= 500 && code < 600 ->
          pure $ Left $ ServerError $ "Server error (status: " <> show code <> ")"
        StatusCode 200 -> pure $ Right body
        StatusCode code -> pure $ Left $ ServerError $ "Unexpected status code: " <>
          show code

-- | Domain-Specific Functions With Error Handling ---------------------------
-- |
-- | These functions provide error-handled versions of the existing domain functions.
-- | They return Either AppError a instead of Maybe a or Either Error (Response a).
-- |
-- | Migration guide:
-- | - Replace getUser with getUserWithError
-- | - Replace getGroups with getGroupsWithError  
-- | - Replace getGroup with getGroupWithError
-- | - Replace changeRole with changeRoleWithError
-- | - Replace removeUser with removeUserWithError
-- | - Replace getDocumentHeader with getDocumentHeaderWithError
-- | - Replace createNewDocument with createNewDocumentWithError
-- | - Replace addGroup with addGroupWithError
-- |
-- | The WithError versions automatically handle:
-- | - Network connection errors (show retry option)
-- | - 401 errors (redirect to login)
-- | - 404 errors (redirect to 404 page or show not found message)
-- | - 403 errors (show access denied message)
-- | - 5xx errors (show server error message with retry)
-- | - JSON decode errors (log for developers, show data error to users)

-- | Fetches the authorized user for a specific group.
-- | Returns Nothing if the user is not existing or not authorized.
getAuthorizedUserWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => GroupID
  -> H.HalogenM st act slots msg m (Either AppError (Maybe FullUserDto))
getAuthorizedUserWithError groupID = do
  userResult <- getUserWithError
  case userResult of
    Left appError -> pure $ Left appError
    Right user -> do
      if (isUserSuperadmin user || user `isAdminOf` groupID) then pure $ Right $ Just
        user
      else pure $ Right Nothing

getGroupsWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => H.HalogenM st act slots msg m (Either AppError (Array GroupOverview))
getGroupsWithError = getJsonWithError (decodeArray decodeJson) "/groups"

getUserGroupsWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => H.HalogenM st act slots msg m (Either AppError (Array GroupOverview))
getUserGroupsWithError = do
  userWithError <- getUserWithError
  case userWithError of
    Left err -> pure $ Left err
    Right u | isUserSuperadmin u -> getGroupsWithError
    Right u -> pure $ Right $ map toGroupOverview $ getAllAdminRoles u

-- | Creates a new document for a specified group.
createNewDocument :: NewDocumentCreateDto -> Aff (Either Error (Response Json))
createNewDocument dto = postJson "/docs" (encodeJson dto)

getDocumentsQueryFromURL :: String -> Aff (Maybe DQ.DocumentQuery)
getDocumentsQueryFromURL url = getFromJSONEndpoint decodeJson url

getUserDocuments :: UserID -> Aff (Maybe (Array DH.DocumentHeader))
getUserDocuments userID = do
  dq <- getDocumentsQueryFromURL $ "/docs?user=" <> userID
  pure $ DQ.getDocuments <$> dq

addGroup :: GroupCreate -> Aff (Either Error (Response Json))
addGroup group = postJson "/groups" (encodeJson group)

-- | Domain-Specific Functions With Error Handling ---------------------------

-- | Error-handling versions of domain-specific functions
getUserWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => H.HalogenM st act slots msg m (Either AppError FullUserDto)
getUserWithError = getJsonWithError decodeJson "/me"

getUserWithIdWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError FullUserDto)
getUserWithIdWithError userId = getJsonWithError decodeJson ("/users/" <> userId)

getGroupWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => GroupID
  -> H.HalogenM st act slots msg m (Either AppError GroupDto)
getGroupWithError groupID = getJsonWithError decodeJson ("/groups/" <> show groupID)

changeRoleWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => GroupID
  -> UserID
  -> Role
  -> H.HalogenM st act slots msg m (Either AppError Unit)
changeRoleWithError groupID userID role =
  putIgnoreWithError ("/roles/" <> show groupID <> "/" <> userID) (encodeJson role)

removeUserWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => GroupID
  -> UserID
  -> H.HalogenM st act slots msg m (Either AppError Unit)
removeUserWithError groupID userID =
  deleteIgnoreWithError ("/roles/" <> show groupID <> "/" <> userID)

getDocumentHeaderWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => DH.DocumentID
  -> H.HalogenM st act slots msg m (Either AppError DH.DocumentHeader)
getDocumentHeaderWithError docID = getJsonWithError decodeJson
  ("/docs/" <> show docID)

createNewDocumentWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => NewDocumentCreateDto
  -> H.HalogenM st act slots msg m (Either AppError Json)
createNewDocumentWithError dto = postJsonWithError Right "/docs" (encodeJson dto)

getDocumentsQueryFromURLWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError DQ.DocumentQuery)
getDocumentsQueryFromURLWithError url = getJsonWithError decodeJson url

getUserDocumentsWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => UserID
  -> H.HalogenM st act slots msg m (Either AppError (Array DH.DocumentHeader))
getUserDocumentsWithError userID = do
  result <- getDocumentsQueryFromURLWithError $ "/docs?user=" <> userID
  case result of
    Left err -> pure $ Left err
    Right dq -> pure $ Right $ DQ.getDocuments dq

addGroupWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => GroupCreate
  -> H.HalogenM st act slots msg m (Either AppError Json)
addGroupWithError group = postJsonWithError Right "/groups" (encodeJson group)

postRenderHtmlWithError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError String)
postRenderHtmlWithError content = do
  result <- handleRequestWithError "/documents/render/html" (postRenderHtml content)
  pure result

-- | PUT-Requests ----------------------------------------------------------
putJson :: String -> Json -> Aff (Either Error (Response Json))
putJson url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.json ("/api" <> url) PUT
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

putIgnore :: String -> Json -> Aff (Either Error (Response Unit))
putIgnore url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.ignore ("/api" <> url) PUT
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | GET-Requests ----------------------------------------------------------

-- | Makes a GET request and expects a String response.
getString :: String -> Aff (Either Error (Response String))
getString url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.string ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | Makes a GET request and expects a JSON response.
getJson :: String -> Aff (Either Error (Response Json))
getJson url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.json ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | Makes a GET request and expects a Document response.
getDocument :: String -> Aff (Either Error (Response Document))
getDocument url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.document ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | Makes a GET request and expects a Blob response.
getBlob :: String -> Aff (Either Error (Response Blob))
getBlob url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.blob ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | Makes a GET request and expects a Null response.
getIgnore :: String -> Aff (Either Error (Response Unit))
getIgnore url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.ignore ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | POST-Requests ---------------------------------------------------------

-- | Makes a POST request with a JSON body and expects a String response.
postString :: String -> Json -> Aff (Either Error (Response String))
postString url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.string ("/api" <> url) POST
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | Makes a POST request to render the content of a paragraph with a String body and expects a String response.
postRenderHtml :: String -> Aff (Either Error (Response String))
postRenderHtml content = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.string
    ("/api/documents/render/html")
    POST
  let request' = fpoRequest { content = Just (RequestBody.json $ fromString content) }
  liftAff $ request driver request'

-- | Makes a POST request with a JSON body and expects a JSON response.
postJson :: String -> Json -> Aff (Either Error (Response Json))
postJson url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.json ("/api" <> url) POST
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | Makes a POST request with a JSON body and expects a Document response.
postDocument :: String -> Json -> Aff (Either Error (Response Document))
postDocument url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.document ("/api" <> url) POST
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | Makes a POST request with a JSON body and expects a Blob response.
postBlob :: String -> Json -> Aff (Either Error (Response Blob))
postBlob url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.blob ("/api" <> url) POST
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | PATCH Requests -----------------------------------------------------

-- | Makes a PATCH request with a JSON body and expects a String response.
patchString :: String -> Json -> Aff (Either Error (Response String))
patchString url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.string ("/api" <> url) PATCH
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | Makes a PATCH request with a JSON body and expects a JSON response.
patchJson :: String -> Json -> Aff (Either Error (Response Json))
patchJson url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.json ("/api" <> url) PATCH
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | DELETE Requests -------------------------------------------------------

-- | Makes a DELETE request and expects a Null response.
deleteIgnore :: String -> Aff (Either Error (Response Unit))
deleteIgnore url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.ignore ("/api" <> url) DELETE
  liftAff $ request driver fpoRequest

-- | Auxiliary Functions -----------------------------------------------------

-- | Extracts the status code from a response.
getStatusCode :: forall a. Response a -> Int
getStatusCode response = extract $ response.status
  where
  extract (StatusCode code) = code
