module FPO.Dto.DocumentDto where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
-- import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
{- <<<<<<< HEAD
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
=======
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import FPO.Dto.TreeDto (RootTree)
>>>>>>> main -}
import Data.Date (canonicalDate, exactDate)
import Data.DateTime (DateTime(..))
import Data.Either (Either(..))
import Data.Enum (toEnum, class BoundedEnum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Time (Time(..))
import Data.Tuple (fst)
import FPO.Dto.TreeDto (RootTree)
-- import Data.String.Read (class Read)
import Parsing (ParserT, runParserT, fail)
import Parsing.String (char, anyTill, rest)

{- newtype NodeWithRef = NodeWithRef
  { id :: Int
  , kind :: String
  , content :: Maybe String
  } -}

-- type DocumentTree = Tree NodeWithRef


type DocumentID = Int
type CommitID = Int

newtype NodeHeader = NodeHeader
  { id :: Int
  , kind :: String
  }

type DocumentTree = RootTree NodeHeader

newtype DocumentHeader = DH
  { group :: Int, headCommit :: Maybe CommitID, id :: DocumentID, name :: String }

newtype DocumentHeaderPlusPermission = DHPP
  { document :: DocumentHeader, documentPermission :: String }

derive instance newtypeUser :: Newtype User _

derive instance newtypeDocumentHeader :: Newtype DocumentHeader _

derive instance newtypeDocumentHeaderPlusPermission ::
  Newtype DocumentHeaderPlusPermission _

derive instance newtypeNodeHeader :: Newtype NodeHeader _

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

instance decodeJsonNodeHeader :: DecodeJson NodeHeader where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "identifier"
    kind <- obj .: "kind"
    pure $ NodeHeader { id, kind }

instance encodeJsonNodeHeader :: EncodeJson NodeHeader where
  encodeJson (NodeHeader { id, kind }) =
    encodeJson
      { content:
          { id
          , kind
          }
      }

-- show instances for debugging purposes
instance showNodeHeader :: Show NodeHeader where
  show (NodeHeader { id, kind }) =
    "NodeHeader { id: " <> show id <> ", kind: " <> show kind <> " }"

decodeDocument :: Json -> Either JsonDecodeError DocumentTree
decodeDocument json = do
  obj <- decodeJson json
  root <- obj .: "root"
  decodeJson root

encodeDocumentTree :: DocumentTree -> Json
encodeDocumentTree tree = encodeJson $ map (\(NodeHeader { id }) -> id) tree
