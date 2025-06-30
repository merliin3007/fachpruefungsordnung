-- | Admin user overview and management page.
-- |
-- | TODO: Implement the actual admin panel functionality, see mockups.
-- |       For a start, this page connects with the backend
-- |       and checks if we're even allowed to access this page, then
-- |       handles the response accordingly.
-- |       Also, this page implements a very simple dummy/mockup user
-- |       management system, which allows us to filter and create users
-- |       (not connected to the backend yet).

module FPO.Page.Admin.Users (component) where

import Prelude

import Data.Argonaut (decodeJson)
import Data.Array (filter, length, replicate, slice)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Pagination as P
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (LoadState(..), getFromJSONEndpoint, getUser)
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Data.UserForOverview (UserForOverview(..), getName)
import FPO.Page.HTML (addButton, addCard, addColumn, emptyEntryText)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import Halogen as H
import Halogen.HTML as HH
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

data Action
  = Initialize
  | Receive (Connected FPOTranslator Unit)
  | DoNothing -- Placeholder for future actions
  | SetPage P.Output
  -- TODO: Of course, we should add dedicated components for the filtering
  --        and creation of users, but for now, we just use these actions to
  --        demonstrate the functionality (mockup!). Or, might be even better,
  --        to add a general component that allows us to create simple forms
  --        with a label, input field(s), and a button. This way, we dont have to
  --        repeat ourselves over and over again.
  | ChangeFilterUsername String
  | ChangeCreateUsername String
  | Filter
  | CreateUser

type State = FPOState
  ( error :: Maybe String
  , page :: Int
  , users :: LoadState (Array UserForOverview)
  , filteredUsers :: Array UserForOverview
  , filterUsername :: String
  , createUsername :: String
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
    , error: Nothing
    , page: 0
    , users: Loading
    , filteredUsers: []
    , filterUsername: ""
    , createUsername: ""
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my5 ] ]
      [ renderUserManagement state
      , HH.div [ HP.classes [ HB.textCenter ] ]
          [ case state.error of
              Just err -> HH.div [ HP.classes [ HB.alert, HB.alertDanger, HB.mt5 ] ]
                [ HH.text err ]
              Nothing -> HH.text ""
          ]
      ]

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      -- TODO: Usually, we would fetch some data here (and handle
      --       the error of missing credentials), but for now,
      --       we just check if the user is an admin and redirect
      --       to a 404 page if not.
      u <- H.liftAff $ getUser
      when (fromMaybe true (not <$> _.isAdmin <$> u)) $ navigate Page404

      maybeUsers <- H.liftAff $ getFromJSONEndpoint decodeJson "/users"
      case maybeUsers of
        Nothing -> do
          H.modify_ _ { error = Just "Failed to load users." }
        Just users -> do
          H.modify_ _ { users = Loaded users, filteredUsers = users }

    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }
    DoNothing -> do
      pure unit
    SetPage (P.Clicked p) -> do
      H.modify_ _ { page = p }
    ChangeFilterUsername username -> do
      H.modify_ _ { filterUsername = username }
    ChangeCreateUsername username -> do
      H.modify_ _ { createUsername = username }
    Filter -> do
      state <- H.get
      filteredUsers <- case state.users of
        Loading -> pure []
        Loaded userList -> pure $ filter
          (\user -> contains (Pattern state.filterUsername) (getName user))
          userList
      H.modify_ _ { filteredUsers = filteredUsers }
    CreateUser -> do
      H.modify_ _ { error = Just "Not implemented yet." }

  renderUserManagement :: State -> H.ComponentHTML Action Slots m
  renderUserManagement state =
    HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
      [ HH.div [ HP.classes [ HB.colSm12, HB.colMd10, HB.colLg9 ] ]
          [ HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
              [ HH.text $ translate (label :: _ "au_userManagement") state.translator
              ]
          , case state.users of
              Loading ->
                HH.div [ HP.classes [ HB.textCenter, HB.mt5 ] ]
                  [ HH.div [ HP.classes [ HB.spinnerBorder, HB.textPrimary ] ] [] ]
              Loaded _ -> renderUserListView state
          ]
      ]

  renderUserListView :: State -> H.ComponentHTML Action Slots m
  renderUserListView state =
    HH.div [ HP.classes [ HB.row, HB.justifyContentAround ] ]
      [ renderFilterBy state
      , renderUserList state
      , renderNewUserForm state
      ]

  renderFilterBy :: State -> H.ComponentHTML Action Slots m
  renderFilterBy state =
    addCard (translate (label :: _ "common_filterBy") state.translator)
      [ HP.classes [ HB.col3 ] ] $ HH.div
      [ HP.classes [ HB.row ] ]
      [ HH.div [ HP.classes [ HB.col ] ]
          [ addColumn
              state.filterUsername
              (translate (label :: _ "common_userName") state.translator)
              (translate (label :: _ "common_userName") state.translator)
              "bi-person"
              HP.InputText
              ChangeFilterUsername
          , addColumn
              ""
              (translate (label :: _ "common_email") state.translator)
              (translate (label :: _ "common_email") state.translator)
              "bi-envelope-fill"
              HP.InputEmail
              (const DoNothing)
          ]
      , HH.div [ HP.classes [ HB.col12, HB.textCenter ] ]
          [ HH.div [ HP.classes [ HB.dInlineBlock ] ]
              [ addButton
                  true
                  "Filter"
                  (Just "bi-funnel")
                  (const Filter)
              ]
          ]
      ]

  -- Creates a list of (dummy) users with pagination.
  renderUserList :: State -> H.ComponentHTML Action Slots m
  renderUserList state =
    addCard (translate (label :: _ "admin_users_listOfUsers") state.translator)
      [ HP.classes [ HB.col5 ] ] $ HH.div_
      [ HH.ul [ HP.classes [ HB.listGroup ] ]
          $ map createUserEntry usrs
              <> replicate (10 - length usrs)
                emptyEntryText
      -- TODO: ^ Artificially inflating the list to 10 entries
      --         allows for a fixed overall height of the list,
      --         but it's not a clean solution at all.
      , HH.slot _pagination unit P.component ps SetPage
      ]
    where
    usrs = slice (state.page * 10) ((state.page + 1) * 10) state.filteredUsers
    ps =
      { pages: P.calculatePageCount (length state.filteredUsers) 10
      , style: P.Compact 1
      , reaction: P.PreservePage
      }

  -- Creates a form to create a new (dummy) user.
  renderNewUserForm :: forall w. State -> HH.HTML w Action
  renderNewUserForm state =
    addCard (translate (label :: _ "admin_users_createNewUser") state.translator)
      [ HP.classes [ HB.col3 ] ] $ HH.div_
      [ HH.div [ HP.classes [ HB.col ] ]
          [ addColumn
              state.createUsername
              (translate (label :: _ "common_userName") state.translator)
              (translate (label :: _ "common_userName") state.translator)
              "bi-person"
              HP.InputText
              ChangeCreateUsername
          , addColumn
              ""
              (translate (label :: _ "common_email") state.translator)
              (translate (label :: _ "common_email") state.translator)
              "bi-envelope-fill"
              HP.InputEmail
              (const DoNothing)
          ]
      , HH.div [ HP.classes [ HB.col12, HB.textCenter ] ]
          [ HH.div [ HP.classes [ HB.dInlineBlock ] ]
              [ addButton
                  true
                  (translate (label :: _ "admin_users_create") state.translator)
                  (Just "bi-plus-circle")
                  (const CreateUser)
              ]
          ]
      ]

  -- Creates a (dummy) user entry for the list.
  createUserEntry :: forall w. UserForOverview -> HH.HTML w Action
  createUserEntry (UserForOverview { userName, userEmail }) =
    HH.li [ HP.classes [ HB.listGroupItem, HB.dFlex, HB.justifyContentBetween ] ]
      [ HH.span [ HP.classes [ HB.col6 ] ] [ HH.text userName ]
      , HH.span [ HP.classes [ HB.col6 ] ] [ HH.text userEmail ]
      ]

