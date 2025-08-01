module FPO.Dto.DocumentDto where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Date (canonicalDate)
import Data.DateTime (DateTime(..))
import Data.Either (Either(..))
import Data.Enum (toEnum, class BoundedEnum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Time (Time(..))
import Data.Tuple (fst)
import FPO.Dto.TreeDto (RootTree)
import Parsing (ParserT, runParserT, fail)
import Parsing.String (char, anyTill, rest)


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

derive newtype instance eqDocDate :: Eq DocDate 

derive newtype instance ordDocDate :: Ord DocDate 

{- newtype DocumentQuery = DQ
  {documents :: (Array NewDocumentHeader), query :: Query} -}
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

--only needed for currently not used newer version
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

--only needed for currently not used newer version
dateParser :: forall m. Monad m => ParserT String m DateTime
dateParser = do
  year <- timeParser '-'
  month <- timeParser '-'
  day <- timeParser 'T'
  hour <- timeParser ':'
  minute <- timeParser ':'
  second <- timeParser '.'
  _ <- rest
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

derive newtype instance decodeJsonQuery :: DecodeJson Query

instance decodeJsonDocumentQuery :: DecodeJson DocumentQuery where
  decodeJson json = do
    obj <- decodeJson json
    d <- obj .: "documents"
    q <- obj .: "query"
    pure $ DQ {documents: d, query: q}

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

----------------------------------------------------------------------
--some values are sometimes missing. This code remedies this until the actual issue is fixed.
--Note: some of the parts of this code segment will be needed afterwards (such as the DocumentQuery type),
--butt will have to be modified first.

newtype Query = Q 
  {group :: Int, user :: Maybe String}  

derive instance newtypeQuery :: Newtype Query _

newtype DocumentQuery = DQ
  {documents :: (Array NewDocumentHeaderOptional), query :: Query}

derive instance newtypeDocumentQuery :: Newtype DocumentQuery _

newtype NewDocumentHeaderOptional = NDHO
  { group :: Int, id :: DocumentID, lastEdited :: Maybe DocDate, lastEditedBy :: Maybe User, name :: String }

derive instance newtypeNewDocumentHeaderOptional :: Newtype NewDocumentHeaderOptional _

--newer version that is not compatible with all API endpoints yet
instance decodeJsonNewHeaderOptional :: DecodeJson NewDocumentHeaderOptional where
  decodeJson json = do
    obj <- decodeJson json
    g <- obj .: "group"
    i <- obj .: "identifier"
    l <- obj .: "lastEdited"
    u <- obj .: "lastEditedBy"
    n <- obj .: "name"
    pure $ NDHO { group: g, id: i, lastEdited: l, lastEditedBy: u, name: n }

getDQDocuments :: DocumentQuery -> Array NewDocumentHeaderOptional
getDQDocuments (DQ dq) = dq.documents
getNDHOGroup :: NewDocumentHeaderOptional -> Int
getNDHOGroup (NDHO ndho) = ndho.group

getNDHOID :: NewDocumentHeaderOptional -> DocumentID
getNDHOID (NDHO ndho) = ndho.id

getNDHOLastEdited :: NewDocumentHeaderOptional -> Maybe DocDate
getNDHOLastEdited (NDHO ndho) = ndho.lastEdited 

getNDHOLastEditedBy :: NewDocumentHeaderOptional -> Maybe User
getNDHOLastEditedBy (NDHO ndho) = ndho.lastEditedBy

getNDHOName :: NewDocumentHeaderOptional -> String
getNDHOName (NDHO ndho) = ndho.name 

convertOptionalToMandatory :: NewDocumentHeaderOptional -> Maybe NewDocumentHeader
convertOptionalToMandatory doc = case getNDHOLastEdited doc of
  Just a -> case getNDHOLastEditedBy doc of
    Just b -> Just $ NDH { group: getNDHOGroup doc
                         , id: getNDHOGroup doc
                         , lastEdited: a
                         , lastEditedBy: b
                         , name: getNDHOName doc }

    Nothing -> Nothing
  Nothing -> Nothing
 ----------------------------------------------------------------------


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
