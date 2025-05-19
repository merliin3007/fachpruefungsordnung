module FPO.Components.Editor where

import Prelude

import Ace (ace, editNode) as Ace
import Ace.Document as Document
import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Types as Types
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick) as HE
import Halogen.HTML.Properties (classes, ref, style) as HP
import Halogen.Themes.Bootstrap5 as HB
import Halogen.Themes.Bootstrap5 (dPrintBlock)
import Effect.Console (log)

type State =
  { key :: Maybe String
  , editor :: Maybe Types.Editor
  , pdfWarning :: Maybe String
  }

data Output
  = ClickedHTTPRequest
  | ClickedQuery (Maybe (Array String))
  | LoadPdf

data Action
  = Init
  | Paragraph
  | Delete
  | MakeRequest
  | QueryEditor
  | ClickLoadPdf
  | ShowWarning
  | Receive (Maybe String)

-- We use a query to get the content of the editor
data Query a = RequestContent (Array String -> a)

type Input = Maybe String

editor :: forall m. MonadEffect m => H.Component Query Input Output m
editor = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  initialState :: State
  initialState = { key: Nothing, editor: Nothing, pdfWarning: Nothing }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1 ] ]
      [ HH.div -- First toolbar

          [ HP.classes [ HB.bgDark, HB.overflowAuto, HB.dFlex, HB.flexRow ] ]
          [ HH.span [ HP.classes [ HB.textWhite ] ] [ HH.text "Toolbar" ]
          , HH.button [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ], HE.onClick $ const MakeRequest ] [ HH.text "Click Me for HTTP request" ]
          , HH.button [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ], HE.onClick $ const QueryEditor ] [ HH.text "Query Editor" ]
          , HH.button [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ], HE.onClick $ const ClickLoadPdf ] [ HH.text "Load PDF" ]
          , if state.pdfWarning == Nothing then HH.div_ []
            else HH.button [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ], HE.onClick $ const ShowWarning ] [ HH.text "Show Warning" ]
          ]
      , HH.div -- Second toolbar

          [ HP.classes [ HB.m1, HB.dFlex, HB.alignItemsCenter, HB.gap2 ] ]
          [ HH.button
              [ HP.classes [ HB.btn, HB.btnOutlinePrimary, HB.btnSm ]
              , HE.onClick \_ -> Paragraph
              ]
              [ HH.i [ HP.classes [ HB.bi, (H.ClassName "bi-paragraph") ] ] []
              , HH.text " Paragraph"
              ]
          , HH.button
              [ HP.classes [ HB.btn, HB.btnOutlinePrimary, HB.btnSm ]
              , HE.onClick \_ -> Delete
              ]
              [ HH.i [ HP.classes [ HB.bi, (H.ClassName "bi-x-lg") ] ] []
              , HH.text " Delete"
              ]
          ]
      , HH.div -- Editor container

          [ HP.ref (H.RefLabel "container")
          , HP.classes [ HB.flexGrow1 ]
          , HP.style "min-height: 0"
          ]
          []

      ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Init -> do
      H.getHTMLElementRef (H.RefLabel "container") >>= traverse_ \el -> do
        editor_ <- H.liftEffect $ Ace.editNode el Ace.ace
        H.modify_ \state -> state { key = Just "Hello", editor = Just editor_ }

    Delete -> do
      H.gets _.editor >>= traverse_ \ed -> do
        H.liftEffect $ do
          row <- Types.getRow <$> Editor.getCursorPosition ed
          document <- Editor.getSession ed >>= Session.getDocument
          Document.removeLines row row document

    Paragraph -> do
      H.gets _.editor >>= traverse_ \ed -> do
        H.liftEffect $ do
          row <- Types.getRow <$> Editor.getCursorPosition ed
          document <- Editor.getSession ed >>= Session.getDocument
          Document.insertLines row [ "Paragraph", "=========" ] document

    MakeRequest -> do
      H.raise ClickedHTTPRequest

    ClickLoadPdf -> do
      H.raise LoadPdf

    -- Because Session does not provide a way to get all lines directly,
    -- we need to take another indirect route to get the lines.
    -- Notice that this extra step is not needed for all js calls.
    -- For example, `Session.getLine` can be called directly.
    QueryEditor -> do
      allLines <- H.gets _.editor >>= traverse \ed -> do
        H.liftEffect $ Editor.getSession ed
          >>= Session.getDocument
          >>= Document.getAllLines
      H.raise (ClickedQuery allLines)

    Receive pdfWarning ->
      H.modify_ \state -> state { pdfWarning = pdfWarning }

    ShowWarning -> do
      warning <- H.gets _.pdfWarning
      H.liftEffect $ log $ show warning

