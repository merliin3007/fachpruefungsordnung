-- | Home page of the application. As of now, this is simply our
-- | "sandbox" for testing components.

module FPO.Page.Home (component) where

import Prelude

import Affjax (printError)
import Affjax.StatusCode (StatusCode(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Unit (unit)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import FPO.Data.Request (getIgnore, getString)
import Type.Proxy (Proxy(..))
import Web.DOM.Document (Document)
import Effect.Aff as Aff
import FPO.Components.Button as Button
import FPO.Components.Editor as Editor
import Halogen as H
import Halogen.Themes.Bootstrap5 as HB
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Components.Preview (Output, PdfState, PdfState(PdfAvailable), PdfState(Empty), PdfState(AskedButError), preview) as Preview
import Components.Preview (Output)

data Action
  = HandleEditor Editor.Output
  | HandlePreview Preview.Output

type State =
  { count :: Int
  , dummyUser :: Maybe String
  , editorContent :: Maybe (Array String)
  , pdf :: Preview.PdfState
  , showWarning :: Boolean
  }

type Slots =
  ( editor :: H.Slot Editor.Query Editor.Output Unit
  , preview :: forall query. H.Slot query Preview.Output Unit
  )

_editor = Proxy :: Proxy "editor"
_preview = Proxy :: Proxy "preview"

component
  :: forall query input output m
   . MonadAff m
  => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: input -> State
  initialState _ = { count: 0, dummyUser: Nothing, editorContent: Nothing, pdf: Preview.Empty, showWarning: false }

  render :: State -> H.ComponentHTML Action Slots m
  render { count, dummyUser, editorContent, pdf, showWarning } =
    HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.p0, HB.overflowHidden ] ]
      [
        -- the _button is for the Proxy to be able to identify it via a term
        -- the 0 is the unique identifier
        -- and button { label: "Click Me" } is the component with its input (communication from parent to child)
        -- in the HH.slot function there is a way to handle messages from the child to the parent (now the HandleButton parameter)
        HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.containerFluid, HB.p0, HB.flexFill, HB.overflowHidden ] ]
          [ HH.div [ HP.classes [ HB.dFlex, HB.flexGrow1, HB.flexRow, HB.g0, HB.overflowHidden ] ]
              [ HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.col6 ] ]
                  [ HH.slot _editor unit Editor.editor (pdfWarningAvailable pdf) HandleEditor ]
              , HH.slot _preview unit Preview.preview { editorContent, pdf, showWarning } HandlePreview
              ]
          ]
      ]

  -- output is when our component communicates with a parent
  -- m is relevant when the component performs effects
  handleAction :: MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
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
        response <- H.liftAff $ getIgnore "/document"
        case response of
          Right { body, headers, status, statusText } -> do
            if status == (StatusCode 201) then H.modify_ _ { pdf = Preview.PdfAvailable }
            else H.modify_ _
              { pdf = Preview.AskedButError
                  ( "Could not load pdf properly. Here should be a detailed warning message"
                      <> "in the future that returned from the compiled pdf. This is just a dummy pdf for now"
                  )
              }
          Left err -> do
            H.liftEffect $ log $ printError err
            H.modify_ _ { pdf = Preview.AskedButError "Error loading PDF." }

      Editor.ClickedShowWarning -> do
        H.modify_ \st -> st { showWarning = not st.showWarning }

    HandlePreview _ ->
      pure unit

pdfWarningAvailable :: Preview.PdfState -> Boolean
pdfWarningAvailable = case _ of
  Preview.AskedButError str -> true
  Preview.Empty -> false
  Preview.PdfAvailable -> false
