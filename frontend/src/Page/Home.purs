-- | Home page of the application. As of now, this is simply our
-- | "sandbox" for testing components.

module FPO.Page.Home (component) where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Request (getString)
import Effect.Aff (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Type.Proxy (Proxy(..))
import Affjax.ResponseFormat as AXRF
import Effect.Aff as Aff
import FPO.Components.Button as Button
import FPO.Components.Editor as Editor
import Halogen as H
import Halogen.Themes.Bootstrap5 as HB
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

data Action
  = Increment
  | Initialize
  | HandleButton Button.Output
  | HandleEditor Editor.Output

type State =
  { count :: Int
  , dummyUser :: Maybe String
  , editorContent :: Maybe (Array String)
  }

type Slots =
  ( button :: forall query. H.Slot query Button.Output Int
  , navbar :: forall query output. H.Slot query output Unit
  , editor :: H.Slot Editor.Query Editor.Output Unit
  )

_button = Proxy :: Proxy "button"
_editor = Proxy :: Proxy "editor"

component
  :: forall query input output m
   . MonadAff m
  => H.Component query input output m
component =
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
    HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.vh100, HB.p0 ] ]
      [
        -- the _button is for the Proxy to be able to identify it via a term
        -- the 0 is the unique identifier
        -- and button { label: "Click Me" } is the component with its input (communication from parent to child)
        -- in the HH.slot function there is a way to handle messages from the child to the parent (now the HandleButton parameter)
        HH.div [ HP.classes [ HB.containerFluid, HB.p0, HB.flexFill ] ]
          [ HH.div [ HP.classes [ HB.row, HB.g0, HB.h100 ] ]
              [ HH.div [ HP.classes [ HB.col6, HB.mh100 ] ]
                  [ HH.slot _editor unit Editor.editor unit HandleEditor ]
              , HH.div [ HP.classes [ HB.col6, HB.textCenter, HB.bgInfoSubtle ] ]
                  [ HH.div_ [ HH.text "Hier sollte die Vorschau sein." ]
                  , HH.div_ [ HH.text $ if dummyUser == Nothing then "Hier kommt nach dem Knopfdruck ein Text" else "Wow, nun haben wir einen dummy User geladen mit dem Namen: " <> fromMaybe "err" dummyUser ]
                  , HH.slot _button 0 Button.button { label: show count } HandleButton
                  , HH.div_
                      [ case editorContent of
                          Nothing ->
                            HH.text "Der Editor ist leer!"
                          Just content ->
                            HH.div_
                              [ HH.text "Editorinhalt:"
                              , HH.div
                                  [ HP.class_ (HH.ClassName "mt-1") ]
                                  [ HH.pre
                                      [ HP.class_ (HH.ClassName "border rounded p-1 bg-light") ]
                                      ( content <#> \line ->
                                          HH.div_ [ HH.text $ preserveEmptyLine line ]
                                      )
                                  ]
                              ]
                      ]
                  ]
              ]
          ]
      ]

  -- Forces the string to be at least one character long.
  preserveEmptyLine :: String -> String
  preserveEmptyLine str = if str == "" then " " else str

  -- the () type means, that we have no child components
  -- output is when our component communicates with a parent
  -- m is relevant when the component performs effects
  handleAction :: MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    HandleButton output -> case output of
      Button.Clicked -> H.modify_ \state -> state { count = 0 }

    HandleEditor output -> case output of
      Editor.ClickedHTTPRequest -> do
        response <- H.liftAff $ getString "/users"
        case response of
          Right { body } ->
            H.modify_ _ { dummyUser = Just body }
          Left _ -> do
            H.modify_ _ { dummyUser = Nothing }

      Editor.ClickedQuery response -> do
        H.modify_ \st -> st { editorContent = response }

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
