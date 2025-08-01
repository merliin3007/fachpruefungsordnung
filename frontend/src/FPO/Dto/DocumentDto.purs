module FPO.Dto.DocumentDto where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
-- import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Date (canonicalDate, exactDate)
import Data.DateTime (DateTime(..))
import Data.Either (Either(..))
import Data.Enum (toEnum, class BoundedEnum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Time (Time(..))
import Data.Tuple (fst)
-- import Data.String.Read (class Read)
import FPO.Dto.TreeDto (Edge(..), Tree(..))
import Parsing (ParserT, runParserT, fail)
import Parsing.String (char, anyTill, rest)

newtype NodeWithRef = NodeWithRef
  { id :: Int
  , kind :: String
  , content :: Maybe String
  }

type DocumentTree = Tree NodeWithRef

type DocumentID = Int
type CommitID = Int

newtype DocumentHeader = DH
  { group :: Int, headCommit :: Maybe CommitID, id :: DocumentID, name :: String }

newtype DocumentHeaderPlusPermission = DHPP
  { document :: DocumentHeader, documentPermission :: String }

derive instance newtypeUser :: Newtype User _

derive instance newtypeDocumentHeader :: Newtype DocumentHeader _

derive instance newtypeDocumentHeaderPlusPermission ::
  Newtype DocumentHeaderPlusPermission _

derive instance newtypeNodeWithRef :: Newtype NodeWithRef _

instance decodeJsonHeader :: DecodeJson DocumentHeader where
  decodeJson json = do
    obj <- decodeJson json
    g <- obj .: "group"
    h <- obj .: "headCommit"
    i <- obj .: "id"
    n <- obj .: "name"
    pure $ DH { group: g, headCommit: h, id: i, name: n }

instance decodeJsonHeaderPlusPermission :: DecodeJson DocumentHeaderPlusPermission where
  decodeJson json = do
    obj <- decodeJson json
    doc <- obj .: "document"
    docPerm <- obj .: "documentPermission"
    pure $ DHPP { document: doc, documentPermission: docPerm }

-------------------------------------------------------
--This code is for the newer version of the API. 
--however, as the API has not been completely adapted to this new version yet, the old version must remain.
--Once all the endpoints have been changed, this should replace some of the other code in this file.

--newer version that is not compatible with all API endpoints yet

newtype User = U
  { id :: String, name :: String }

newtype DocDate = DocDate DateTime

derive newtype instance ordDocDate :: Ord DocDate 
newtype NewDocumentHeader = NDH
  { group :: Int, id :: DocumentID, lastEdited :: DocDate, lastEditedBy :: User, name :: String }

derive instance newtypeNewDocumentHeader :: Newtype NewDocumentHeader _

--only needed for curretnly not used newer version
instance decodeJsonUser :: DecodeJson User where
  decodeJson json = do
    obj <- decodeJson json
    i <- obj .: "identifier"
    n <- obj .: "name"
    pure $ U { id: i, name: n }

--only needed for curretnly not used newer version
timeParser :: forall m a. BoundedEnum a => Monad m => Char -> ParserT String m a
timeParser c = do
  res <- anyTill (char c)
  h res
    where
      h string = case fromString $ fst (string) of
        Nothing -> fail "can't parse number"
        Just num -> case toEnum num of
          Nothing -> fail "not valid"
          Just a -> pure a

{- --only needed for curretnly not used newer version
dateParser :: forall m. Monad m => ParserT String m DateTime
dateParser = do
  year <- timeParser '-'
  month <- timeParser '-'
  day <- timeParser 'T'
  hour <- timeParser ':'
  minute <- timeParser ':'
  second <- timeParser 'Z'
  case (exactDate year month day) of
    Nothing -> fail "not valid"
    Just a -> case (toEnum 0) of
      Nothing -> fail "not valid"
      Just b -> pure $ DateTime a (Time hour minute second b) -}

--only needed for curretnly not used newer version
{- dateParser :: forall m. Monad m => ParserT String m DateTime
dateParser = do
  year <- timeParser '-'
  month <- timeParser '-'
  day <- timeParser 'T'
  string <- rest
  case (toEnum 1) of
    Nothing -> fail "not valid"
    Just d -> case (toEnum 1) of
      Nothing -> fail "not valid"
      Just e -> case (toEnum 1) of
        Nothing -> fail "not valid"
        Just f -> case (toEnum 0) of
          Nothing -> fail "not valid"
          Just g -> pure $ DateTime (canonicalDate year month day) (Time d e f g) -}

--only needed for curretnly not used newer version
dateParser :: forall m. Monad m => ParserT String m DateTime
dateParser = do
  year <- timeParser '-'
  month <- timeParser '-'
  day <- timeParser 'T'
  hour <- timeParser ':'
  minute <- timeParser ':'
  second <- timeParser '.'
  string <- rest
  case (toEnum 0) of
    Nothing -> fail "not valid"
    Just g -> pure $ DateTime (canonicalDate year month day) (Time hour minute second g)

--only needed for currently not used newer version
instance decodeJsonDateTime :: DecodeJson DocDate where
  decodeJson json = do
    obj <- decodeJson json
    result <- runParserT obj dateParser 
    case result of 
      Left _ -> Left (UnexpectedValue json)
      Right datetime -> Right $ DocDate datetime

--newer version that is not compatible with all API endpoints yet
instance decodeJsonNewHeader :: DecodeJson NewDocumentHeader where
  decodeJson json = do
    obj <- decodeJson json
    g <- obj .: "group"
    i <- obj .: "identifier"
    l <- obj .: "lastEdited"
    u <- obj .: "lastEditedBy"
    n <- obj .: "name"
    pure $ NDH { group: g, id: i, lastEdited: l, lastEditedBy: u, name: n }

getNDHName :: NewDocumentHeader -> String
getNDHName (NDH ndh) = ndh.name

getNDHID :: NewDocumentHeader -> Int
getNDHID (NDH ndh) = ndh.id 

getNDHLastEdited :: NewDocumentHeader -> DocDate
getNDHLastEdited (NDH ndh) = ndh.lastEdited 

docDateToDateTime :: DocDate -> DateTime
docDateToDateTime (DocDate date) = date

-------------------------------------------------------

getDHName :: DocumentHeader -> String
getDHName (DH dh) = dh.name

getDHID :: DocumentHeader -> Int
getDHID (DH dh) = dh.id 

getDHHeadCommit :: DocumentHeader -> Maybe CommitID
getDHHeadCommit (DH dh) = dh.headCommit

getDHPPName :: DocumentHeaderPlusPermission -> String
getDHPPName (DHPP dhpp) = getDHName dhpp.document

getDHPPID :: DocumentHeaderPlusPermission -> Int
getDHPPID (DHPP dhpp) = getDHID dhpp.document

instance decodeJsonNodeWithRef :: DecodeJson NodeWithRef where
  decodeJson json = do
    obj <- decodeJson json
    inner <- obj .: "content" -- neu: extrahiere das innere Objekt
    id <- inner .: "id"
    kind <- inner .: "kind"
    content <- inner .: "content"
    pure $ NodeWithRef { id, kind, content }

instance encodeJsonNodeWithRef :: EncodeJson NodeWithRef where
  encodeJson (NodeWithRef { id, kind, content }) =
    encodeJson
      { content:
          { id
          , kind
          , content
          }
      }

-- show instances for debugging purposes
instance showNodeWithRef :: Show NodeWithRef where
  show (NodeWithRef { id, kind, content }) =
    "NodeWithRef { id: " <> show id <> ", kind: " <> show kind <> ", content: "
      <> show content
      <> " }"

decodeDocument :: Json -> Either JsonDecodeError DocumentTree
decodeDocument json = do
  obj <- decodeJson json
  body <- obj .: "body"
  root <- body .: "root"
  decodeJson root

-- Encode instances
-- | Erzeugt ein JSON-Objekt mit Dummy-Daten passend zur Upload-Schnittstelle
encodeCreateCommit :: DocumentTree -> Json
encodeCreateCommit tree =
  encodeJson
    { info:
        { author: "00000000-0000-0000-0000-000000000000"
        , message: "Initial commit"
        , parents: [ 1 ]
        }
    , root: encodeTree tree
    }

encodeTree :: DocumentTree -> Json
encodeTree Empty = encodeJson {}
encodeTree (Node { node, children }) =
  let
    { id, kind, content } = unwrap node
  in
    encodeJson
      { node:
          { id
          , kind
          , content: fromMaybe "" content
          }
      , edges: map encodeEdge children
      }

encodeEdge :: Edge NodeWithRef -> Json
encodeEdge (Edge { title, child }) =
  encodeJson
    { title
    , child: encodeTree child
    }
