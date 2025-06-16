-- | Home page of the application. As of now, this is simply our
-- | "sandbox" for testing components.
-- |
-- | Right now, it displays a list of mock projects and allows
-- | the user to navigate to the editor by clicking on the title.
-- | The user must be logged in to see the projects.

module FPO.Page.Home (component) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import FPO.Components.Table.Head as TH
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
import Type.Proxy (Proxy(..))

_tablehead = Proxy :: Proxy "tablehead"

type Input = Unit

data Sorting = TitleAsc | TitleDesc | LastUpdatedAsc | LastUpdatedDesc

data Action
  = Initialize
  | NavLogin
  | ViewProject String
  | Receive (Connected FPOTranslator Input)
  | DoNothing
  | ChangeSorting TH.Output

type State = FPOState
  (user :: Maybe User, projects :: Array Project)

-- | Model for Projects (mockup).
type Project =
  { name :: String
  , description :: String
  , version :: String
  , collaborators :: Int
  , updated :: String
  }

type Slots =
  ( tablehead :: forall q. H.Slot q TH.Output Unit
  )

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
    { user: Nothing, translator: fromFpoTranslator context, projects: mockProjects }

  render
    :: State
    -> H.ComponentHTML Action Slots m
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
    :: MonadAff m
    => Action
    -> H.HalogenM State Action Slots output m Unit
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
    ChangeSorting (TH.Clicked title order) -> do
      state <- H.get
      projects <- pure $ case title of
        "Title" ->
          TH.sortByF order (\a b -> compare a.name b.name) state.projects
        "Last Updated" ->
          TH.sortByF order (\a b -> compare a.updated b.updated) state.projects
        _ -> state.projects -- Ignore other columns.

      H.modify_ _ { projects = projects }
      pure unit

  -- | Renders the overview of projects for the user.
  renderProjectsOverview :: State -> H.ComponentHTML Action Slots m
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
  renderProjectOverview :: State -> H.ComponentHTML Action Slots m
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
  renderProjectTable :: State -> H.ComponentHTML Action Slots m
  renderProjectTable state =
    HH.table
      [ HP.classes [ HB.table, HB.tableHover, HB.tableBordered ] ]
      [ HH.slot _tablehead unit TH.component tableCols ChangeSorting
      , HH.tbody
          []
          (map (renderProjectRow state) state.projects)
      ]
    where
    tableCols = TH.createTableColumns
      [ { title: "Title"
        , style: Just TH.Alpha
        }
      , { title: "Last Updated"
        , style: Just TH.Numeric
        }
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
