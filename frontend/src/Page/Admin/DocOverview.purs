-- | Overview of Documents belonging to Group

module FPO.Page.Admin.DocOverview (component) where

import Prelude

-- | Copied over. Redundant imports to be removed later
import Data.Array (filter, length, replicate, slice, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Pagination as P
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (getUser)
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Page.HTML (addButton, addCard, addColumn, emptyEntryGen)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Type.Proxy (Proxy(..))

_pagination = Proxy :: Proxy "pagination"

type Slots =
  ( pagination :: H.Slot P.Query P.Output Unit
  )

type Group = String

type Document = 

data Action
  = Initialize
  | Receive (Connected FPOTranslator Unit)
  | SetPage P.Output
  | ChangeFilterDocumentName String
  | CreateDocument
  -- | Used to set the document name for deletion confirmation
  -- | before the user confirms the deletion using the modal.
  | RequestDeleteDocument String
  -- | Actually deletes the document after confirmation.
  | ConfirmDeleteDocument String
  | CancelDeleteDocument
  | Filter

type State = FPOState
  ( error :: Maybe String
  , page :: Int
  , group :: Group
  , documents :: Array Document 
  , filteredGroups :: Array Group
  , groupNameCreate :: String
  , groupNameFilter :: String
  -- | This is used to store the group name for deletion confirmation.
  , requestDelete :: Maybe String
  )