-- | Overview of Documents belonging to Group

module FPO.Page.Admin.DocOverview (component) where

import Prelude

-- | Copied over. Redundant imports to be removed later
import Data.Array (filter, length, replicate, slice, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Data.Time.Duration
  ( class Duration
  , Days(..)
  , Hours(..)
  , Seconds(..)
  , negateDuration
  , toDuration
  )
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Pagination as P
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (getUser)
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Page.Home (adjustDateTime)
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

type GroupID = Int

-- preliminary data type. So far everything seems to use different data for documents, this should be changed.
type Document =
    { body ::
      { name :: String
      , text :: String}
    , header ::
      { creationTs :: DateTime
      , id :: Int
      }
    }

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
  , groupID :: GroupID
  , documents :: Array Document
  , filteredDocuments :: Array Doucment
  , currentTime :: Maybe DateTime
  , documentNameFilter :: String
  -- | This is used to store the document name for deletion confirmation.
  , requestDelete :: Maybe String
  )

  -- | Admin panel page component.
component
  :: forall query output m
   . MonadStore Store.Action Store.Store m
  => MonadAff m
  => Navigate m
  => H.Component query Unit output m
component =
  connect selectTranslator $ H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }
  where
  initialState :: Connected FPOTranslator Unit -> State
  initialState { context } =
    { translator: fromFpoTranslator context
    , page: 0
    -- this is only a placeholder, to be changed once connection to backend is established
    , groupID: 1
    , documents: []
    , documentNameFilter: ""
    , filteredDocuments: []
    , error: Nothing
    , requestDelete: Nothing
    , currentTime: Nothing
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my5 ] ]
      $
        ( case state.requestDelete of
            Just documentName -> [ deleteConfirmationModal documentName ]
            Nothing -> []
        )

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      u <- liftAff $ getUser
      when (fromMaybe true (not <$> _.isAdmin <$> u)) $
        navigate Page404
      now <- liftEffect nowDateTime
      H.modify_ _
        { documents = mockDocuments now
        , currentTime = Just now
        }

  mockDocuments :: DateTime -> Array Document
  mockDocuments now =
    [
      { body:
        { name: "Doc1"
        , text: "This is the first Document"
        }
      , header:
        {
          creationTs: now
          id: 1
        }
      }
      ,
      { body:
        { name: "Doc2"
        , text: "This is the second Document"
        }
      , header:
        {
          creationTs: adjustDateTime (Days (5.0)) now
          id: 2
        }
      }
    ]
