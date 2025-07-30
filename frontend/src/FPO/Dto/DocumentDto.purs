module FPO.Dto.DocumentDto where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
-- import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Date (exactDate)
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
import Parsing.String (char, anyTill)

newtype NodeWithRef = NodeWithRef
  { id :: Int
  , kind :: String
  , content :: Maybe String
  }

type DocumentTree = Tree NodeWithRef

type DocumentID = Int
type CommitID = Int

{- newtype DocumentHeader = DH
  { group :: Int, headCommit :: Maybe CommitID, id :: DocumentID, name :: String } -}

-- the identifier is an UUID, but as the frontend probably doesn't need this for much,
-- it will remain as a strring, as this is what the backend sends.
newtype User = U
  { identifier :: String, name :: String }

newtype DocDate = DocDate DateTime

newtype DocumentHeader = DH
  { group :: Int, identifier :: DocumentID, lastEdited :: DocDate, lastEditedBy :: User, name :: String }

newtype DocumentHeaderPlusPermission = DHPP
  { document :: DocumentHeader, documentPermission :: String }

derive instance newtypeUser :: Newtype User _

derive instance newtypeDocumentHeader :: Newtype DocumentHeader _

derive instance newtypeDocumentHeaderPlusPermission ::
  Newtype DocumentHeaderPlusPermission _

derive instance newtypeNodeWithRef :: Newtype NodeWithRef _

{- instance decodeJsonHeader :: DecodeJson DocumentHeader where
  decodeJson json = do
    obj <- decodeJson json
    g <- obj .: "group"
    h <- obj .: "headCommit"
    i <- obj .: "id"
    n <- obj .: "name"
    pure $ DH { group: g, headCommit: h, id: i, name: n } -}

instance decodeJsonUser :: DecodeJson User where
  decodeJson json = do
    obj <- decodeJson json
    i <- obj .: "identifier"
    n <- obj .: "name"
    pure $ U { identifier: i, name: n }

{- instance readDateTime :: Read DateTime where
  read string =  -}

{- dateParser :: forall m. ParserT String m DateTime
dateParser = do
  year <- anyTill (char '-')
  month <- anyTill (char '-')
  day <- anyTill (char 'T')
  hour <- anyTill (char ':')
  minute <- anyTill (char ':')
  second <- anyTill (char 'Z')
  pure $ DateTime (exactDate (h year) (h month) (h day)) (Time (h hour) (h minute) (h second) (toEnum 0))
    where
      h string = case fromString $ fst (string) of
        Nothing -> Nothing
        Just num -> case toEnum num of
          Nothing -> -}

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
      Just b -> pure $ DateTime a (Time hour minute second b)

instance decodeJsonDateTime :: DecodeJson DocDate where
  decodeJson json = do
    obj <- decodeJson json
    result <- runParserT obj dateParser 
    case result of 
      Left _ -> Left (UnexpectedValue json)
      Right datetime -> Right $ DocDate datetime

instance decodeJsonHeader :: DecodeJson DocumentHeader where
  decodeJson json = do
    obj <- decodeJson json
    g <- obj .: "group"
    i <- obj .: "identifier"
    l <- obj .: "lastEdited"
    u <- obj .: "lastEditedBy"
    n <- obj .: "name"
    pure $ DH { group: g, identifier: i, lastEdited: l, lastEditedBy: u, name: n }

instance decodeJsonHeaderPlusPermission :: DecodeJson DocumentHeaderPlusPermission where
  decodeJson json = do
    obj <- decodeJson json
    doc <- obj .: "document"
    docPerm <- obj .: "documentPermission"
    pure $ DHPP { document: doc, documentPermission: docPerm }

getDHName :: DocumentHeader -> String
getDHName (DH dh) = dh.name

getDHID :: DocumentHeader -> Int
getDHID (DH dh) = dh.identifier 

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
