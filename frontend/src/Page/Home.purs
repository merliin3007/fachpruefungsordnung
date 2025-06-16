-- | Home page of the application. As of now, this is simply our
-- | "sandbox" for testing components.
-- |
-- | Right now, it displays a list of mock projects and allows
-- | the user to navigate to the editor by clicking on the title.
-- | The user must be logged in to see the projects.

module FPO.Page.Home (component) where

import Prelude

import Data.Array (sortBy)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (getUser)
import FPO.Data.Route (Route(..))
import FPO.Data.Store (User)
import FPO.Data.Store as Store
import FPO.Page.HTML (addCard, addColumn)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)

type Input = Unit

data Sorting = TitleAsc | TitleDesc | LastUpdatedAsc | LastUpdatedDesc

data Action
  = Initialize
  | NavLogin
  | ViewProject String
  | Receive (Connected FPOTranslator Input)
  | DoNothing
  -- TODO: Move to component!
  --       We should perhaps add a component for table headers,
  --       which allows us to sort by clicking on the header.
  --       The sorting logic should be handled in the parent component (this one),
  --       but all the rendering logic should be in the table header component.
  --       This way, we can reuse the component in other places and we can remove
  --       some repeated code from this component.
  | ChangeSortingTitle
  | ChangeSortingLastUpdated

type State = FPOState
  (user :: Maybe User, sorting :: Sorting)

-- Model for Projects
type Project =
  { name :: String
  , description :: String
  , version :: String
  , collaborators :: Int
  , updated :: String
  }

component
  :: forall query output m
   . MonadAff m
  => Navigate m
  => MonadStore Store.Action Store.Store m
  => H.Component query Input output m
component =
  connect selectTranslator $ H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { initialize = Just Initialize
        , handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
  where
  initialState :: Connected FPOTranslator Input -> State
  initialState { context } =
    { user: Nothing, translator: fromFpoTranslator context, sorting: TitleAsc }

  render
    :: forall slots
     . State
    -> H.ComponentHTML Action slots m
  render state =
    HH.div
      [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.p0, HB.overflowHidden ]
      ]
      [ HH.div_
          [ HH.h1 [ HP.classes [ HB.textCenter, HB.mt5 ] ]
              [ HH.text $ translate (label :: _ "common_home") state.translator ]
          , HH.div [ HP.classes [ HB.dropdownDivider, HB.mb4 ] ] []
          , renderProjectsOverview state
          ]
      ]

  handleAction
    :: forall slots
     . MonadAff m
    => Action
    -> H.HalogenM State Action slots output m Unit
  handleAction = case _ of
    Initialize -> do
      store <- getStore
      u <- liftAff getUser
      H.modify_ _
        { user = u, translator = fromFpoTranslator store.translator }
    Receive { context } -> H.modify_ _ { translator = fromFpoTranslator context }
    ViewProject projectName -> do
      log $ "Routing to editor for project " <> projectName
      navigate Editor
    NavLogin -> do
      updateStore $ Store.SetLoginRedirect (Just Home)
      navigate Login
    DoNothing ->
      pure unit
    ChangeSortingTitle -> do
      H.modify_ \state ->
        case state.sorting of
          TitleAsc -> state { sorting = TitleDesc }
          TitleDesc -> state { sorting = TitleAsc }
          _ -> state { sorting = TitleAsc }
    ChangeSortingLastUpdated -> do
      H.modify_ \state ->
        case state.sorting of
          LastUpdatedAsc -> state { sorting = LastUpdatedDesc }
          LastUpdatedDesc -> state { sorting = LastUpdatedAsc }
          _ -> state { sorting = LastUpdatedAsc }

  -- | Renders the overview of projects for the user.
  renderProjectsOverview :: forall w. State -> HH.HTML w Action
  renderProjectsOverview state = case state.user of
    Just _ -> HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
      [ addCard
          (translate (label :: _ "home_yourProjects") state.translator)
          [ HP.classes [ HB.col8 ] ]
          (renderProjectOverview state)
      ]
    Nothing -> HH.p
      [ HP.classes [ HB.textCenter, HB.mt5 ] ]
      [ HH.text $ translate (label :: _ "home_pleaseLogInA") state.translator
      , HH.a
          [ HE.onClick $ const $ NavLogin
          , HP.classes [ HB.textDecorationUnderline, HB.textDark ]
          , HP.style "cursor: pointer;"
          ]
          [ HH.text $ translate (label :: _ "home_toLogin") state.translator ]
      , HH.text $ translate (label :: _ "home_pleaseLogInB") state.translator
      ]

  -- | Search bar and list of projects.
  renderProjectOverview :: forall w. State -> HH.HTML w Action
  renderProjectOverview state =
    HH.div [ HP.classes [ HB.container ] ]
      [ HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
          [ HH.div [ HP.classes [ HB.col6 ] ]
              [ addColumn
                  ""
                  ""
                  "Search for Projects"
                  "bi-search"
                  HP.InputText
                  (const $ DoNothing) -- TODO: Implement search/filter functionality
              ]
          , HH.div [ HP.classes [ HB.col12 ] ]
              [ renderProjectTable state ]
          ]
      ]

  -- | Renders the list of projects.
  renderProjectTable :: forall w. State -> HH.HTML w Action
  renderProjectTable state =
    HH.table
      [ HP.classes [ HB.table, HB.tableHover, HB.tableBordered ] ]
      [ HH.thead
          [ HP.classes [ HB.textCenter ] ]
          [ HH.tr [ HP.class_ (HB.tableSecondary) ]
              [ HH.th
                  [ HP.classes [ getSortingTitleStyle state ]
                  ]
                  [ HH.div
                      [ HE.onClick $ const ChangeSortingTitle
                      , HP.style "display: inline-flex; cursor: pointer;"
                      ]
                      [ HH.i
                          [ HP.classes
                              [ H.ClassName $ getTitleSortingIcon state, HB.me1 ]
                          ]
                          []
                      , HH.text "Title"
                      ]
                  ]
              , HH.th
                  [ HP.classes [ getSortingLastUpdatedStyle state ]
                  ]
                  [ HH.div
                      [ HE.onClick $ const ChangeSortingLastUpdated
                      , HP.style "display: inline-flex; cursor: pointer;"
                      ]
                      [ HH.i
                          [ HP.classes
                              [ H.ClassName $ getLastUpdatedSortingIcon state
                              , HB.me1
                              ]
                          ]
                          []
                      , HH.text "Last Updated"
                      ]
                  ]
              ]
          ]
      , HH.tbody
          []
          (map (renderProjectRow state) $ sortProjects state mockProjects)
      ]

  -- | Renders a single project row in the table.
  renderProjectRow :: forall w. State -> Project -> HH.HTML w Action
  renderProjectRow _ project =
    HH.tr
      [ HE.onClick $ const $ ViewProject project.name
      , HP.style "cursor: pointer;"
      ]
      [ HH.td [ HP.classes [ HB.textCenter ] ]
          [ HH.text project.name ]
      , HH.td [ HP.classes [ HB.textCenter ] ]
          [ HH.text project.updated ]
      ]

  getTitleSortingIcon :: State -> String
  getTitleSortingIcon state =
    case state.sorting of
      TitleAsc -> "bi-sort-alpha-down"
      _ -> "bi-sort-alpha-up"

  getLastUpdatedSortingIcon :: State -> String
  getLastUpdatedSortingIcon state =
    case state.sorting of
      LastUpdatedAsc -> "bi-sort-numeric-down"
      _ -> "bi-sort-numeric-up"

  getSortingTitleStyle state =
    case state.sorting of
      TitleAsc -> HB.bgSecondarySubtle
      TitleDesc -> HB.bgSecondarySubtle
      _ -> HB.bgBodySecondary

  getSortingLastUpdatedStyle state =
    case state.sorting of
      LastUpdatedAsc -> HB.bgSecondarySubtle
      LastUpdatedDesc -> HB.bgSecondarySubtle
      _ -> HB.bgBodySecondary

  sortProjects :: State -> Array Project -> Array Project
  sortProjects state projects =
    case state.sorting of
      TitleAsc -> sortBy (\a b -> compare a.name b.name) projects
      TitleDesc -> sortBy (\a b -> compare b.name a.name) projects
      LastUpdatedAsc -> sortBy (\a b -> compare a.updated b.updated) projects -- TODO: `updated` should be a date, not a string.
      LastUpdatedDesc -> sortBy (\a b -> compare b.updated a.updated) projects --      This is not sensible yet.

  mockProjects :: Array Project
  mockProjects =
    [ { name: "FPO Informatik 1-Fach M.Sc. 2025"
      , description: "Hier gibt es Zusatzinformationen!"
      , version: "1.2"
      , collaborators: 3
      , updated: "6 hours ago"
      }
    , { name: "FPO Elektrotechnik und Informationstechnik B.Sc. 2022"
      , description:
          (<>)
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed eiusmod tempor"
            "incidunt Hello! ut labore et dolore magna aliqua. Ut enim ad minim veniam"
      , version: "4.8"
      , collaborators: 4
      , updated: "3 days ago"
      }
    ]
