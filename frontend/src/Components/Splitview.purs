-- It has 3 split views: a sidebar, an editor, and a preview area. 
-- Between each of the views, there are resizers that allow the user to adjust the width 
-- of each section. The sidebar contains a table of contents (TOC) with clickable entries 
-- that jump to specific sections in the editor. The editor allows users to edit content, 
-- and the preview area displays the output based on the editor's content.

module FPO.Component.Splitview where

import Prelude

import Components.Preview
  ( Output
  , Query
      ( TellClickedHttpRequest
      , GotEditorQuery
      , TellLoadPdf
      , TellLoadUploadedPdf
      , TellShowOrHideWarning
      )
  , preview
  ) as Preview
import Data.Array (findIndex, head, range, updateAt)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Editor as Editor
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB
import Type.Proxy (Proxy(Proxy))
import Web.HTML as Web.HTML
import Web.HTML.Window as Web.HTML.Window
import Web.UIEvent.MouseEvent (MouseEvent, clientX)

data DragTarget = ResizeLeft | ResizeRight

derive instance eqDragTarget :: Eq DragTarget

type TOCEntry =
  { id :: Int
  , name :: String
  , content :: Maybe (Array String)
  }

type Output = Unit
type Input = Unit
data Query a = UnitQuery a

data Action
  = Init
  -- Resizing Actions
  | StartResize DragTarget MouseEvent
  | StopResize MouseEvent
  | HandleMouseMove MouseEvent
  | ToggleSidebar
  | JumpToSection TOCEntry
  -- Toolbar buttons
  | ClickedHTTPRequest
  | SaveSection
  | QueryEditor
  | ClickLoadPdf
  | ShowWarning
  -- Query Output
  | HandleEditor Editor.Output
  | HandlePreview Preview.Output

type State =
  { dragTarget :: Maybe DragTarget

  -- TOC: Table of Contents
  , tocEntries :: Array TOCEntry

  -- Store the width values as ratios of the total width
  -- TODO: Using the ratios to keep the ratio, when resizing the window

  -- Instead of setting the width directly to mouse position, calculate a delta 
  -- for a smoother and correct resize experience with the start positions
  , startMouseRatio :: Number
  , startSidebarRatio :: Number
  , startMiddleRatio :: Number

  -- The current widths of the sidebar and middle content (as percentage ratios)
  , sidebarRatio :: Number
  , middleRatio :: Number

  -- The last expanded sidebar width, used to restore the sidebar when toggling
  , lastExpandedSidebarRatio :: Number

  , editorContent :: Maybe (Array String)
  , hasResizedSidebar :: Boolean
  , pdfWarningAvailable :: Boolean
  , pdfWarningIsShown :: Boolean
  }

type Slots =
  ( editor :: H.Slot Editor.Query Editor.Output Unit
  , preview :: H.Slot Preview.Query Preview.Output Unit
  )

_editor = Proxy :: Proxy "editor"
_preview = Proxy :: Proxy "preview"

splitview :: forall query m. MonadAff m => H.Component query Input Output m
splitview = H.mkComponent
  { initialState: \_ ->
      { dragTarget: Nothing
      , tocEntries: []
      , startMouseRatio: 0.0
      , startSidebarRatio: 0.0
      , startMiddleRatio: 0.0
      , sidebarRatio: 0.2
      , middleRatio: 0.4
      , lastExpandedSidebarRatio: 0.2
      , editorContent: Nothing
      , hasResizedSidebar: false
      , pdfWarningAvailable: false
      , pdfWarningIsShown: false
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Init, handleAction = handleAction }
  }
  where

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div_
      [ -- First Toolbar
        HH.div
          [ HP.classes [ HB.bgDark, HB.overflowAuto, HB.dFlex, HB.flexRow ] ]
          [ HH.button
              [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ]
              , HE.onClick $ const ToggleSidebar
              ]
              [ HH.text "[≡]" ]
          , HH.span [ HP.classes [ HB.textWhite, HB.px2 ] ] [ HH.text "Toolbar" ]
          , HH.button
              [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ]
              , HE.onClick $ const ClickedHTTPRequest
              ]
              [ HH.text "Click Me for HTTP request" ]
          , HH.button
              [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ]
              , HE.onClick $ const SaveSection
              ]
              [ HH.text "Save" ]
          , HH.button
              [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ]
              , HE.onClick $ const QueryEditor
              ]
              [ HH.text "Query Editor" ]
          , HH.button
              [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ]
              , HE.onClick $ const ClickLoadPdf
              ]
              [ HH.text "Load PDF" ]
          , if not state.pdfWarningAvailable then HH.div_ []
            else HH.button
              [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ]
              , HE.onClick $ const ShowWarning
              ]
              [ HH.text
                  ((if state.pdfWarningIsShown then "Hide" else "Show") <> " Warning")
              ]
          ]
      , renderSplit state
      ]

  renderSplit :: State -> H.ComponentHTML Action Slots m
  renderSplit state =
    HH.div
      -- Set mouse event
      [ HE.onMouseMove HandleMouseMove
      , HE.onMouseUp StopResize
      , HP.classes [ HB.dFlex ]
      , HP.style "height: 100vh; position: relative; user-select: none;"
      ]
      [ -- Sidebar
        HH.div
          [ HP.classes [ HB.overflowAuto, HB.p1 ]
          , HP.style $
              "flex: 0 0 " <> show (state.sidebarRatio * 100.0) <>
                "%; box-sizing: border-box; min-width: 6ch; background:rgb(229, 241, 248);"
          ]
          [ HH.div_
              ( map
                  ( \{ id, name, content } ->
                      HH.div
                        [ HP.title ("Jump to section " <> name)
                        , HP.style
                            "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; padding: 0.25rem 0;"
                        ]
                        [ HH.span
                            [ HE.onClick \_ -> JumpToSection { id, name, content }
                            , HP.classes [ HB.textTruncate ]
                            , HP.style
                                "cursor: pointer; display: inline-block; min-width: 6ch;"
                            ]
                            [ HH.text name ]
                        ]
                  )
                  state.tocEntries
              )
          ]

      -- Resizer
      , HH.div
          [ HE.onMouseDown (StartResize ResizeLeft)
          , HP.style "width: 5px; cursor: col-resize; background: #ccc;"
          ]
          []

      -- Editor
      , HH.div
          [ HP.classes [ HB.dFlex, HB.flexColumn ]
          , HP.style $
              "flex: 0 0 " <> show (state.middleRatio * 100.0) <>
                "%; box-sizing: border-box; min-height: 0; overflow: hidden;"
          ]
          [ HH.slot _editor unit Editor.editor unit HandleEditor ]

      -- Resizer
      , HH.div
          [ HE.onMouseDown (StartResize ResizeRight)
          , HP.style "width: 5px; cursor: col-resize; background: #ccc;"
          ]
          []

      -- Preview
      , HH.div
          [ HP.classes [ HB.dFlex, HB.flexColumn ]
          , HP.style $
              "flex: 1 1 "
                <> show ((1.0 - state.sidebarRatio - state.middleRatio) * 100.0)
                <> "%; box-sizing: border-box; min-height: 0; overflow: hidden;"
          ]
          [ HH.slot _preview unit Preview.preview
              { editorContent: state.editorContent }
              HandlePreview
          ]
      ]

  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of

    Init -> do
      let
        -- Create initial TOC entries
        entries = map
          ( \n ->
              { id: n
              , name: "§" <> show n <> " This is Paragraph " <> show n
              , content: Just [ "This is the content of §" <> show n ]
              }
          )
          (range 1 11)
        -- Put first entry in editor
        firstEntry = case head entries of
          Nothing -> { id: -1, name: "No Entry", content: Just [ "" ] }
          Just entry -> entry
      H.modify_ \st -> do
        st
          { tocEntries = entries
          , editorContent = Just [ "This is the initial content of the editor." ]
          }
      H.tell _editor unit (Editor.ChangeSection firstEntry)

    -- Set the left resizer to a set position on the left side
    -- Toggle the sidebar again, wihtout resizing this resizer put it back in last position
    ToggleSidebar -> do
      win <- H.liftEffect Web.HTML.window
      width <- H.liftEffect $ Web.HTML.Window.innerWidth win
      let w = toNumber width
      state <- H.get
      -- The sidebar is "closed"
      if state.sidebarRatio * w <= 85.0 then do
        let
          target = state.lastExpandedSidebarRatio
          delta = target - (85.0 / w)
        H.modify_ \st -> st
          { sidebarRatio = target
          , middleRatio = max 0.1 (st.middleRatio - delta)
          , hasResizedSidebar = true
          }
      -- "close" sidebar
      else do
        let
          newSidebar = 85.0 / w
          delta = state.sidebarRatio - newSidebar
        H.modify_ \st -> st
          { sidebarRatio = newSidebar
          , middleRatio = max 0.1 (st.middleRatio + delta)
          , lastExpandedSidebarRatio =
              if st.hasResizedSidebar then st.lastExpandedSidebarRatio
              else st.sidebarRatio
          , hasResizedSidebar = false
          }

    -- Resizing as long as mouse is hold down on window
    StartResize which mouse -> do
      win <- H.liftEffect Web.HTML.window
      intWidth <- H.liftEffect $ Web.HTML.Window.innerWidth win
      let
        x = toNumber $ clientX mouse
        width = toNumber intWidth
        ratioX = x / width
      H.modify_ \st -> st
        { dragTarget = Just which
        , startMouseRatio = ratioX
        , startSidebarRatio = st.sidebarRatio
        , startMiddleRatio = st.middleRatio
        }

    -- Stop resizing, when mouse is released
    StopResize _ ->
      H.modify_ \st -> st
        { dragTarget = Nothing
        , hasResizedSidebar = true
        }

    -- While mouse is hold down, resizer move to position of mouse
    -- (with certain rules)
    HandleMouseMove mouse -> do
      win <- H.liftEffect Web.HTML.window
      intWidth <- H.liftEffect $ Web.HTML.Window.innerWidth win
      let
        x = toNumber $ clientX mouse
        width = toNumber intWidth
        ratioX = x / width
      mt <- H.gets _.dragTarget
      mx <- H.gets _.startMouseRatio
      case mt of
        Just ResizeLeft -> do
          s <- H.gets _.startSidebarRatio
          m <- H.gets _.startMiddleRatio
          let
            rawSidebarRatio = s + (ratioX - mx)
            maxSidebarRatio = s + m - (85.0 / width)
            -- enforce minimum width to show at least ~6 characters
            clampedSidebarRatio = max (70.0 / width)
              (min maxSidebarRatio rawSidebarRatio)
            newSidebar = clampedSidebarRatio
            newMiddle = s + m - clampedSidebarRatio
          when (newMiddle * width >= 85.0) do
            H.modify_ \st -> st
              { sidebarRatio = newSidebar
              , middleRatio = newMiddle
              , lastExpandedSidebarRatio =
                  if newSidebar * width > 85.0 then newSidebar
                  else st.lastExpandedSidebarRatio
              , hasResizedSidebar = true
              }

        Just ResizeRight -> do
          m <- H.gets _.startMiddleRatio
          let
            newMiddleRatio = max 0.1 (m + (ratioX - mx))
          H.modify_ \st -> st { middleRatio = newMiddleRatio }

        _ -> pure unit

    -- Change the content of current § section in the editor
    JumpToSection section -> do
      H.tell _editor unit Editor.SaveSection
      H.tell _editor unit (Editor.ChangeSection section)

    -- Toolbar button actions

    ClickedHTTPRequest -> H.tell _preview unit Preview.TellClickedHttpRequest

    SaveSection -> H.tell _editor unit Editor.SaveSection

    QueryEditor -> do
      H.tell _editor unit Editor.SaveSection
      H.tell _editor unit Editor.QueryEditor

    ShowWarning -> do
      H.modify_ \st -> st { pdfWarningIsShown = not st.pdfWarningIsShown }
      H.tell _preview unit Preview.TellShowOrHideWarning

    ClickLoadPdf -> do
      H.modify_ \st -> st { pdfWarningAvailable = true }
      H.tell _editor unit Editor.LoadPdf
      H.tell _preview unit Preview.TellLoadPdf

    -- Query handler

    HandleEditor output -> case output of
      Editor.ClickedQuery response -> H.tell _preview unit
        (Preview.GotEditorQuery response)
      Editor.SavedSection section -> do
        state <- H.get
        -- update the TOC entry received from the editor in state
        case findIndex (\e -> e.id == section.id) state.tocEntries of
          Nothing -> pure unit
          Just idx ->
            case updateAt idx section state.tocEntries of
              Nothing -> pure unit
              Just updatedEntries ->
                H.modify_ \st -> st { tocEntries = updatedEntries }

      Editor.SendPDF mURL -> H.tell _preview unit (Preview.TellLoadUploadedPdf mURL)

    HandlePreview _ -> pure unit

