-- | This module provides functions for making HTTP requests, including GET and POST requests.
-- | It supports various response formats such as String, JSON, Document, and Blob.
-- | The functions use the Affjax library to handle asynchronous HTTP requests.

module FPO.Data.Request
  ( addGroup
  , changeRole
  , createNewDocument
  , deleteIgnore
  , getAuthorizedUser
  , getBlob
  , getDocumentHeader
  , getDocument
  , getDocumentsQueryFromURL
  , getGroup
  , getGroups
  , getIgnore
  , getJson
  , getString
  , getTextElemHistory
  , getUserDocuments
  , getUserGroups
  , getUser
  , getUserWithId
  , patchJson
  , patchString
  , postBlob
  , postDocument
  , postJson
  , postRenderHtml
  , postString
  , putIgnore
  , putJson
  , removeUser
  , getFromJSONEndpoint
  , LoadState(..)
  ) where

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
import FPO.Data.Store as Store
import FPO.Dto.CreateDocumentDto (NewDocumentCreateDto)
import FPO.Dto.DocumentDto.DocDate as DD
import FPO.Dto.DocumentDto.DocumentHeader as DH
import FPO.Dto.DocumentDto.Query as DQ
import FPO.Dto.DocumentDto.TextElement as TE
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
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
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
handleRequest'
  :: forall a st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String -- URL for error context
  -> Aff (Either Error (Response a))
  -> H.HalogenM st act slots msg m (Either AppError a)
handleRequest' url requestAction = do
  response <- H.liftAff requestAction
  case response of
    Left err -> do
      pure $ Left $ NetworkError $ printAjaxError "Connection failed" err
    Right { body, status } -> do
      case status of
        StatusCode 401 -> do
          let
            errorMessage =
              if url == "/login" then "Invalid credentials" else "Session expired."
            appError = AuthError errorMessage
          store <- getStore
          updateStore $ Store.SetLoginRedirect store.currentRoute
          handleAppError appError
          updateStore $ Store.AddError appError
          pure $ Left appError
        StatusCode 403 -> do
          handleAppError AccessDeniedError
          updateStore $ Store.AddError AccessDeniedError
          pure $ Left AccessDeniedError
        StatusCode 404 -> do
          handleAppError (NotFoundError url)
          updateStore $ Store.AddError (NotFoundError url)
          pure $ Left $ NotFoundError url
        StatusCode 405 -> do
          handleAppError (MethodNotAllowedError url "Unknown")
          updateStore $ Store.AddError (MethodNotAllowedError url "Unknown")
          pure $ Left $ MethodNotAllowedError url "Unknown"
        StatusCode code | code >= 500 && code < 600 -> do
          handleAppError (ServerError $ "Server error (status: " <> show code <> ")")
          updateStore $ Store.AddError
            (ServerError $ "Server error (status: " <> show code <> ")")
          pure $ Left $ ServerError $ "Server error (status: " <> show code <> ")"
        StatusCode 200 ->
          pure $ Right body
        StatusCode 201 ->
          pure $ Right body
        StatusCode 204 ->
          pure $ Right body
        StatusCode code -> do
          handleAppError (ServerError $ "Unexpected status code: " <> show code)
          updateStore $ Store.AddError
            (ServerError $ "Unexpected status code: " <> show code)
          pure $ Left $ ServerError $ "Unexpected status code: " <> show code

-- | Wrapper specifically for JSON responses with decode step
handleJsonRequest'
  :: forall a st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => (Json -> Either JsonDecodeError a)
  -> String
  -> Aff (Either Error (Response Json))
  -> H.HalogenM st act slots msg m (Either AppError a)
handleJsonRequest' decode url requestAction = do
  result <- handleRequest' url requestAction
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
handleUnitRequest
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> Aff (Either Error (Response Unit))
  -> H.HalogenM st act slots msg m (Either AppError Unit)
handleUnitRequest = handleRequest'

getFromJSONEndpoint
  :: forall a. (Json -> Either JsonDecodeError a) -> String -> Aff (Maybe a)
getFromJSONEndpoint decode url = do
  response <- getJson' url
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
getString
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError String)
getString url = handleRequest' url (getString' url)

getJson
  :: forall a st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => (Json -> Either JsonDecodeError a)
  -> String
  -> H.HalogenM st act slots msg m (Either AppError a)
getJson decode url = handleJsonRequest' decode url (getJson' url)

getBlob
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError Blob)
getBlob url = handleRequest' url (getBlob' url)

getDocument
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError Document)
getDocument url = handleRequest' url (getDocument' url)

getIgnore
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError Unit)
getIgnore url = handleUnitRequest url (getIgnore' url)

postJson
  :: forall a st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => (Json -> Either JsonDecodeError a)
  -> String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError a)
postJson decode url body = handleJsonRequest' decode url
  (postJson' url body)

postString
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError String)
postString url body = handleRequest' url (postString' url body)

postBlob
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError Blob)
postBlob url body = handleRequest' url (postBlob' url body)

postDocument
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError Document)
postDocument url body = handleRequest' url (postDocument' url body)

putJson
  :: forall a st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => (Json -> Either JsonDecodeError a)
  -> String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError a)
putJson decode url body = handleJsonRequest' decode url
  (putJson' url body)

putIgnore
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError Unit)
putIgnore url body = handleUnitRequest url (putIgnore' url body)

patchJson
  :: forall a st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => (Json -> Either JsonDecodeError a)
  -> String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError a)
patchJson decode url body = handleJsonRequest' decode url
  (patchJson' url body)

patchString
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError String)
patchString url body = handleRequest' url (patchString' url body)

deleteIgnore
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError Unit)
deleteIgnore url = handleUnitRequest url (deleteIgnore' url)

-- | Fetches the authorized user for a specific group.
-- | Returns Nothing if the user is not existing or not authorized.
getAuthorizedUser
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => GroupID
  -> H.HalogenM st act slots msg m (Either AppError (Maybe FullUserDto))
getAuthorizedUser groupID = do
  userResult <- getUser
  case userResult of
    Left appError -> pure $ Left appError
    Right user -> do
      if (isUserSuperadmin user || user `isAdminOf` groupID) then pure $ Right $ Just
        user
      else pure $ Right Nothing

getGroups
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => H.HalogenM st act slots msg m (Either AppError (Array GroupOverview))
getGroups = getJson (decodeArray decodeJson) "/groups"

getUserGroups
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => H.HalogenM st act slots msg m (Either AppError (Array GroupOverview))
getUserGroups = do
  user <- getUser
  case user of
    Left err -> pure $ Left err
    Right u | isUserSuperadmin u -> getGroups
    Right u -> pure $ Right $ map toGroupOverview $ getAllAdminRoles u

-- | Error-handling versions of domain-specific functions
getUser
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => H.HalogenM st act slots msg m (Either AppError FullUserDto)
getUser = getJson decodeJson "/me"

getUserWithId
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError FullUserDto)
getUserWithId userId = getJson decodeJson ("/users/" <> userId)

getGroup
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => GroupID
  -> H.HalogenM st act slots msg m (Either AppError GroupDto)
getGroup groupID = getJson decodeJson ("/groups/" <> show groupID)

changeRole
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => GroupID
  -> UserID
  -> Role
  -> H.HalogenM st act slots msg m (Either AppError Unit)
changeRole groupID userID role =
  putIgnore ("/roles/" <> show groupID <> "/" <> userID) (encodeJson role)

removeUser
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => GroupID
  -> UserID
  -> H.HalogenM st act slots msg m (Either AppError Unit)
removeUser groupID userID =
  deleteIgnore ("/roles/" <> show groupID <> "/" <> userID)

getDocumentHeader
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => DH.DocumentID
  -> H.HalogenM st act slots msg m (Either AppError DH.DocumentHeader)
getDocumentHeader docID = getJson decodeJson ("/docs/" <> show docID)

createNewDocument
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => NewDocumentCreateDto
  -> H.HalogenM st act slots msg m (Either AppError DH.DocumentHeader)
createNewDocument dto = postJson decodeJson "/docs" (encodeJson dto)

getDocumentsQueryFromURL
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError DQ.DocumentQuery)
getDocumentsQueryFromURL url = getJson decodeJson url

getTextElemHistory
  :: DH.DocumentID
  -> TE.TextElementID
  -> DD.DocDate
  -> Int
  -> Aff (Maybe TE.FullTextElementHistory)
getTextElemHistory dID tID date limit =
  getFromJSONEndpoint
    decodeJson
    ( "/docs/" <> show dID <> "/text/" <> show tID <> "/history?before="
        <> DD.toStringFormat date
        <> "&limit="
        <> show limit
    )

getUserDocuments
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => UserID
  -> H.HalogenM st act slots msg m (Either AppError (Array DH.DocumentHeader))
getUserDocuments userID = do
  result <- getDocumentsQueryFromURL $ "/docs?user=" <> userID
  case result of
    Left err -> pure $ Left err
    Right dq -> pure $ Right $ DQ.getDocuments dq

addGroup
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => GroupCreate
  -> H.HalogenM st act slots msg m (Either AppError GroupID)
addGroup group = postJson decodeJson "/groups" (encodeJson group)

postRenderHtml
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError String)
postRenderHtml content = do
  result <- handleRequest' "/docs/render/html" (postRenderHtml' content)
  pure result

-- | PUT-Requests ----------------------------------------------------------
putJson' :: String -> Json -> Aff (Either Error (Response Json))
putJson' url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.json ("/api" <> url) PUT
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

putIgnore' :: String -> Json -> Aff (Either Error (Response Unit))
putIgnore' url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.ignore ("/api" <> url) PUT
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | GET-Requests ----------------------------------------------------------

-- | Makes a GET request and expects a String response.
getString' :: String -> Aff (Either Error (Response String))
getString' url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.string ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | Makes a GET request and expects a JSON response.
getJson' :: String -> Aff (Either Error (Response Json))
getJson' url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.json ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | Makes a GET request and expects a Document response.
getDocument' :: String -> Aff (Either Error (Response Document))
getDocument' url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.document ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | Makes a GET request and expects a Blob response.
getBlob' :: String -> Aff (Either Error (Response Blob))
getBlob' url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.blob ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | Makes a GET request and expects a Null response.
getIgnore' :: String -> Aff (Either Error (Response Unit))
getIgnore' url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.ignore ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | POST-Requests ---------------------------------------------------------

-- | Makes a POST request with a JSON body and expects a String response.
postString' :: String -> Json -> Aff (Either Error (Response String))
postString' url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.string ("/api" <> url) POST
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | Makes a POST request to render the content of a paragraph with a String body and expects a String response.
postRenderHtml' :: String -> Aff (Either Error (Response String))
postRenderHtml' content = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.string
    ("/api/docs/render/html")
    POST
  let request' = fpoRequest { content = Just (RequestBody.json $ fromString content) }
  liftAff $ request driver request'

-- | Makes a POST request with a JSON body and expects a JSON response.
postJson' :: String -> Json -> Aff (Either Error (Response Json))
postJson' url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.json ("/api" <> url) POST
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | Makes a POST request with a JSON body and expects a Document response.
postDocument' :: String -> Json -> Aff (Either Error (Response Document))
postDocument' url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.document ("/api" <> url) POST
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | Makes a POST request with a JSON body and expects a Blob response.
postBlob' :: String -> Json -> Aff (Either Error (Response Blob))
postBlob' url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.blob ("/api" <> url) POST
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | PATCH Requests -----------------------------------------------------

-- | Makes a PATCH request with a JSON body and expects a String response.
patchString' :: String -> Json -> Aff (Either Error (Response String))
patchString' url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.string ("/api" <> url) PATCH
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | Makes a PATCH request with a JSON body and expects a JSON response.
patchJson' :: String -> Json -> Aff (Either Error (Response Json))
patchJson' url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.json ("/api" <> url) PATCH
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | DELETE Requests -------------------------------------------------------

-- | Makes a DELETE request and expects a Null response.
deleteIgnore' :: String -> Aff (Either Error (Response Unit))
deleteIgnore' url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.ignore ("/api" <> url) DELETE
  liftAff $ request driver fpoRequest
