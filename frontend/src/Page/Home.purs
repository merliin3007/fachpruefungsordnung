-- | Home page of the application. As of now, this is simply our
-- | "sandbox" for testing components.

module FPO.Page.Home (component) where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Data.Request (getDocument, getString)
import Type.Proxy (Proxy(..))
import Web.DOM.Document (Document)
import Affjax.Web (get) as AX
import Affjax.ResponseFormat (document) as AXRF
import Effect.Aff as Aff
import FPO.Components.Button as Button
import FPO.Components.Editor as Editor
import Halogen as H
import Halogen.Themes.Bootstrap5 as HB
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Effect.Console (log)
import Affjax.StatusCode (StatusCode(..))

data Action
  = Increment
  | Initialize
  | HandleButton Button.Output
  | HandleEditor Editor.Output

data PdfState
  = Empty
  | AskedButError -- to load a default PDF
  | PdfAvailable Document

type State =
  { count :: Int
  , dummyUser :: Maybe String
  , editorContent :: Maybe (Array String)
  , pdf :: PdfState
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
  initialState _ = { count: 0, dummyUser: Nothing, editorContent: Nothing, pdf: Empty }

  render :: State -> H.ComponentHTML Action Slots m
  render { count, dummyUser, editorContent, pdf } =
    HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.p0, HB.overflowHidden ] ]
      [
        -- the _button is for the Proxy to be able to identify it via a term
        -- the 0 is the unique identifier
        -- and button { label: "Click Me" } is the component with its input (communication from parent to child)
        -- in the HH.slot function there is a way to handle messages from the child to the parent (now the HandleButton parameter)
        HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.containerFluid, HB.p0, HB.flexFill, HB.overflowHidden ] ]
          [ HH.div [ HP.classes [ HB.dFlex, HB.flexGrow1, HB.flexRow, HB.g0, HB.overflowHidden ] ]
              [ HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.col6 ] ]
                  [ HH.slot _editor unit Editor.editor unit HandleEditor ]
              , case pdf of
                  Empty -> HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.col6, HB.textCenter, HB.bgInfoSubtle, HB.overflowHidden ] ]
                    [ HH.div_ [ HH.text "Hier sollte die Vorschau sein." ]
                    , HH.div_ [ HH.text $ if dummyUser == Nothing then "Hier kommt nach dem Knopfdruck ein Text" else "Wow, nun haben wir einen dummy User geladen mit dem Namen: " <> fromMaybe "err" dummyUser ]
                    , HH.slot _button 0 Button.button { label: show count } HandleButton
                    , HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.overflowHidden ] ]
                        [ case editorContent of
                            Nothing ->
                              HH.text "Der Editor ist leer!"
                            Just content ->
                              HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.overflowHidden ] ]
                                [ HH.text "Editorinhalt:"
                                , HH.div
                                    [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HH.ClassName "mt-1", HB.overflowAuto ] ]
                                    [ HH.pre
                                        [ HP.classes [ HB.flexGrow1, HH.ClassName "border rounded p-1 bg-light", HB.h100 ] ]
                                        ( content <#> \line ->
                                            HH.div_ [ HH.text $ preserveEmptyLine line ]
                                        )
                                    ]
                                ]
                        ]
                    ]
                  AskedButError -> HH.div [ HP.classes [ HB.col6, HB.textCenter, HB.bgInfoSubtle ] ]
                    [ HH.embed [ HP.src "./src/Page/document.pdf", HP.classes [ HB.w100, HB.h100 ] ] [] ]
                  PdfAvailable _ -> HH.div_ [ HH.text "Ich habe ein PDF" ]
              ]
          ]
      ]

  -- Forces the string to be at least one character long.
  preserveEmptyLine :: String -> String
  preserveEmptyLine str = if str == "" then " " else str

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

      Editor.LoadPdf -> do
        response <- H.liftAff $ getDocument "/document" -- TODO this wont work for now, but have to be inplemented in the backend
        case response of
          Right { body, headers, status, statusText } -> do
            if status == (StatusCode 200) then H.modify_ _ { pdf = PdfAvailable body }
            else H.modify_ _ { pdf = AskedButError }
          Left _ -> do
            H.modify_ _ { pdf = AskedButError }

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
