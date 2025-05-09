module Main
  where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web (get) as AX
import Button as Button
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Editor (Query(..), editor) as Editor
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick) as HB
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.Themes.Bootstrap5 (bgDark, bgInfoSubtle, btn, btnSm, btnSuccess, col6, containerFluid, dFlex, flexColumn, flexFill, g0, h100, mh100, p0, row, textCenter, textWhite, vh100) as HB
import Halogen.VDom.Driver (runUI)
import Navbar as Navbar
import Type.Proxy (Proxy(..))
import Web.UIEvent.MouseEvent (MouseEvent)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI parent unit body

data Action
    = Increment
    | Initialize
    | HandleButton Button.Output
    | MakeRequest MouseEvent
    | QueryEditor

type State =
    { count :: Int
    , dummyUser :: Maybe String
    , editorContent :: Maybe (Array String)
    }

type Slots = 
  ( button :: forall query. H.Slot query Button.Output Int
  , navbar :: forall query output. H.Slot query output Unit
  , editor :: forall output. H.Slot Editor.Query output Unit
  )

_button = Proxy :: Proxy "button"
_navbar = Proxy :: Proxy "navbar"
_editor = Proxy :: Proxy "editor"

parent :: forall query input output m. MonadAff m => H.Component query input output m
parent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval 
      { handleAction = handleAction
      , initialize = Just Initialize
      }
    }
  where
  initialState :: input -> State
  initialState _ = { count: 0, dummyUser: Nothing, editorContent: Nothing }

  render :: State -> H.ComponentHTML Action Slots m
  render { count, dummyUser, editorContent } =
    HH.div [ HP.id "root", HP.classes [ HB.dFlex, HB.flexColumn, HB.vh100, HB.p0 ] ]
      [ HH.slot_ _navbar unit Navbar.navbar unit
      -- the _button is for the Proxy to be able to identify it via a term
      -- the 0 is the unique identifier
      -- and button { label: "Click Me" } is the component with its input (communication from parent to child)
      -- in the HH.slot function there is a way to handle messages from the child to the parent (now the HandleButton parameter)
      , HH.div [ HP.classes [ HB.containerFluid, HB.p0, HB.flexFill ] ]
          [ HH.div [ HP.classes [ HB.row, HB.g0, HB.h100 ] ]
            [ HH.div [ HP.classes [ HB.col6, HB.mh100 ] ]
                [ HH.div [ HP.style "height: 2rem", HP.classes [ HB.bgDark ] ]
                    [ HH.span [ HP.classes [ HB.textWhite ] ] [ HH.text "Toolbar" ]
                    , HH.button [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ], HB.onClick MakeRequest ] [ HH.text "Click Me for HTTP request" ]
                    , HH.button [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ], HB.onClick $ const QueryEditor ] [ HH.text "Query Editor" ] ]
                , HH.slot_ _editor unit Editor.editor unit ]
            , HH.div [ HP.classes [ HB.col6, HB.textCenter, HB.bgInfoSubtle ] ]
                [ HH.div_ [ HH.text "Hier sollte die Vorschau sein." ]
                , HH.div_ [ HH.text $ if dummyUser == Nothing then "Hier kommt nach dem Knopfdruck ein Text" else "Wow, nun haben wir einen dummy User geladen mit dem Namen: " <> fromMaybe "err" dummyUser ]
                , HH.slot _button 0 Button.button { label: show count } HandleButton
                , HH.div_ [
                    case editorContent of
                      Nothing -> 
                        HH.text "Der Editor ist leer!"
                      Just content ->
                        HH.div_ [
                          HH.text "Editorinhalt:",
                          HH.div 
                            [ HP.class_ (HH.ClassName "mt-1") ] 
                            [ HH.pre 
                                [ HP.class_ (HH.ClassName "border rounded p-1 bg-light") ]
                                (content <#> \line -> 
                                  HH.div_ [ HH.text line ]
                                )
                            ] 
                        ]
                  ]
                ]
            ]
          ]
      ]

  -- the () type means, that we have no child components 
  -- output is when our component communicates with a parent
  -- m is relevant when the component performs effects
  handleAction :: MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    HandleButton output -> case output of 
      Button.Clicked -> H.modify_ \state -> state { count = 0 }
    Increment -> H.modify_ \state -> state { count = state.count + 1 }
    Initialize -> do 
      { emitter, listener } <- H.liftEffect HS.create
      void $ H.subscribe emitter 
      void 
        $ H.liftAff
        $ Aff.forkAff
        $ forever do 
          Aff.delay $ Milliseconds 1000.0
          H.liftEffect $ HS.notify listener Increment
    MakeRequest _ -> do
        response <- H.liftAff $ AX.get AXRF.string "https://random-data-api.com/api/v2/users"
        case response of
            Right { body } ->
                H.modify_ _ { dummyUser = Just body }
            Left _ -> do
                H.modify_ _ { dummyUser = Nothing }
    QueryEditor -> do
        response <- H.request _editor unit Editor.RequestContent
        H.modify_ \st -> st { editorContent = response }