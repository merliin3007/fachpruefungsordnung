-- | It has 3 split views: a sidebar, an editor, and a preview area.
-- | Between each of the views, there are resizers that allow the user to adjust the width
-- | of each section. The sidebar contains a table of contents (TOC) with clickable entries
-- | that jump to specific sections in the editor. The editor allows users to edit content,
-- | and the preview area displays the output based on the editor's content.

module FPO.Component.Splitview where

import Prelude

import Data.Array
  ( cons
  , deleteAt
  , find
  , head
  , insertAt
  , null
  , snoc
  , uncons
  , updateAt
  , (!!)
  )
import Data.Either (Either(..))
import Data.Formatter.DateTime (Formatter)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import FPO.Components.Comment as Comment
import FPO.Components.CommentOverview as CommentOverview
import FPO.Components.Editor as Editor
import FPO.Components.Preview as Preview
import FPO.Components.TOC as TOC
import FPO.Data.Request as Request
import FPO.Data.Store as Store
import FPO.Dto.DocumentDto.DocumentHeader (DocumentID)
import FPO.Dto.DocumentDto.DocumentTree as DT
import FPO.Dto.DocumentDto.TreeDto (Edge(..), RootTree(..), Tree(..), modifyNodeRootTree, findRootTree)
import FPO.Types
  ( CommentSection
  , TOCEntry
  , TOCTree
  , documentTreeToTOCTree
  , emptyTOCEntry
  , findTOCEntry
  , findTitleTOCEntry
  , replaceTOCEntry
  , timeStampsVersions
  , tocTreeToDocumentTree
  )
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import Type.Proxy (Proxy(Proxy))
import Web.Event.Event (EventType(..), stopPropagation)
import Web.HTML as Web.HTML
import Web.HTML.Window as Web.HTML.Window
import Web.UIEvent.MouseEvent (MouseEvent, clientX)

data DragTarget = ResizeLeft | ResizeRight

derive instance eqDragTarget :: Eq DragTarget

type Output = Unit
type Input = DocumentID
data Query a = UnitQuery a

data Action
  = Init
  -- Resizing Actions
  | StartResize DragTarget MouseEvent
  | StopResize MouseEvent
  | HandleMouseMove MouseEvent
  -- Toggle buttons
  | ToggleComment
  | ToggleCommentOverview Boolean
  | ToggleSidebar
  | TogglePreview
  -- Query Output
  | HandleComment Comment.Output
  | HandleCommentOverview CommentOverview.Output
  | HandleEditor Editor.Output
  | HandlePreview Preview.Output
  | HandleTOC TOC.Output
  | GET
  | POST
  | ModifyVersionMapping Int (Maybe Int)

type State =
  { docID :: DocumentID
  , mDragTarget :: Maybe DragTarget

  -- Store the width values as ratios of the total width
  -- TODO: Using the ratios to keep the ratio, when resizing the window
  --      But how do we get the event of window resize?

  -- Instead of setting the width directly to mouse position, calculate a delta
  -- for a smoother and correct resize experience with the start positions
  , startMouseRatio :: Number
  , startSidebarRatio :: Number
  , startPreviewRatio :: Number

  -- The current widths of the sidebar and middle content (as percentage ratios)
  , sidebarRatio :: Number
  , previewRatio :: Number

  -- The last expanded sidebar width, used to restore the sidebar when toggling
  , lastExpandedSidebarRatio :: Number
  , lastExpandedPreviewRatio :: Number

  -- There are 2 ways to send content to preview:
  -- 1. This editorContent is sent through the slot in renderPreview
  -- 2. Throuth QueryEditor of the Editor, where the editor collects its content and sends it
  --   to the preview component.
  -- TODO: Which one to use?
  , renderedHtml :: Maybe String

  -- Store tocEntries and send some parts to its children components
  , tocEntries :: TOCTree

  -- store for each element which version should be shown Nothing means the most recent version should be shown
  , versionMapping :: (RootTree ElemVersion)

  -- How the timestamp has to be formatted
  , mTimeFormatter :: Maybe Formatter

  -- Boolean flags for UI state
  , sidebarShown :: Boolean
  , tocShown :: Boolean
  , commentOverviewShown :: Boolean
  , commentShown :: Boolean
  , previewShown :: Boolean
  , pdfWarningAvailable :: Boolean
  , pdfWarningIsShown :: Boolean
  }

type ElemVersion = {elementID :: Int, versionID :: Maybe Int}

type Slots =
  ( comment :: H.Slot Comment.Query Comment.Output Unit
  , commentOverview :: H.Slot CommentOverview.Query CommentOverview.Output Unit
  , editor :: H.Slot Editor.Query Editor.Output Unit
  , preview :: H.Slot Preview.Query Preview.Output Unit
  , toc :: H.Slot TOC.Query TOC.Output Unit
  )

_comment = Proxy :: Proxy "comment"
_commentOverview = Proxy :: Proxy "commentOverview"
_editor = Proxy :: Proxy "editor"
_preview = Proxy :: Proxy "preview"
_toc = Proxy :: Proxy "toc"

splitview
  :: forall query m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => H.Component query Input Output m
splitview = H.mkComponent
  { initialState: \docID ->
      { mDragTarget: Nothing
      , startMouseRatio: 0.0
      , startSidebarRatio: 0.0
      , startPreviewRatio: 0.0
      , sidebarRatio: 0.2
      , previewRatio: 0.4
      , lastExpandedSidebarRatio: 0.2
      , lastExpandedPreviewRatio: 0.4
      , renderedHtml: Nothing
      , tocEntries: Empty
      , versionMapping: Empty
      , mTimeFormatter: Nothing
      , sidebarShown: true
      , tocShown: true
      , commentOverviewShown: false
      , commentShown: false
      , previewShown: true
      , pdfWarningAvailable: false
      , pdfWarningIsShown: false
      , docID: docID
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Init, handleAction = handleAction }
  }
  where

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div_
      [ renderSplit state ]

  renderSplit :: State -> H.ComponentHTML Action Slots m
  renderSplit state =
    -- We have to manually shrink the size of those elements, otherwise if they overflow the bottom
    -- of the elements wont be visible anymore
    let
      navbarHeight :: Int
      navbarHeight = 56 -- px, height of the navbar

      toolbarHeight :: Int
      toolbarHeight = 31 -- px, height of the toolbar
    in
      HH.div
        [ HE.onMouseMove HandleMouseMove
        , HE.onMouseUp StopResize
        , HE.onMouseLeave StopResize
        , HP.classes [ HB.dFlex, HB.overflowHidden ]
        , HP.style
            ( "height: calc(100vh - " <> show (navbarHeight + toolbarHeight) <>
                "px); max-height: 100%;"
            )
        ]
        ( -- TOC Sidebar
          renderSidebar state
            <>
              [ -- Editor
                HH.div
                  [ HP.style $ "position: relative; flex: 0 0 "
                      <> show
                        ((1.0 - state.sidebarRatio - state.previewRatio) * 100.0)
                      <> "%;"
                  ]
                  [ -- The actual editor area
                    HH.div
                      [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow0 ]
                      , HP.style
                          "height: 100%; box-sizing: border-box; min-height: 0; overflow: hidden;"
                      ]
                      [ HH.slot _editor unit Editor.editor state.docID HandleEditor ]
                  ]
              ]
            <>
              -- Preview Sectioin
              renderPreview state
        )

  -- Render both TOC and Comment but make them visable depending of the flags
  -- Always keep them load to not load them over and over again
  renderSidebar :: State -> Array (H.ComponentHTML Action Slots m)
  renderSidebar state =
    [ -- TOC
      HH.div
        [ HP.classes [ HB.overflowAuto, HB.p1 ]
        , HP.style $
            "flex: 0 0 " <> show (state.sidebarRatio * 100.0)
              <>
                "%; box-sizing: border-box; min-width: 6ch; background:rgb(229, 241, 248); position: relative;"
              <>
                if
                  state.sidebarShown
                    && not state.commentOverviewShown
                    && not state.commentShown
                    && state.tocShown then
                  ""
                else
                  "display: none;"
        ]
        [ HH.slot _toc unit TOC.tocview state.docID HandleTOC ]
    -- Comment
    , HH.div
        [ HP.classes [ HB.overflowAuto, HB.p1 ]
        , HP.style $
            "flex: 0 0 " <> show (state.sidebarRatio * 100.0)
              <>
                "%; box-sizing: border-box; min-width: 6ch; background:rgb(229, 241, 248); position: relative;"
              <>
                if state.sidebarShown && state.commentShown then
                  ""
                else
                  "display: none;"
        ]
        [ HH.button
            [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlineSecondary ]
            , HP.style
                "position: absolute; \
                \top: 0.5rem; \
                \right: 0.5rem; \
                \background-color: #fdecea; \
                \color: #b71c1c; \
                \padding: 0.2rem 0.4rem; \
                \font-size: 0.75rem; \
                \line-height: 1; \
                \border: 1px solid #f5c6cb; \
                \border-radius: 0.2rem; \
                \z-index: 10;"
            , HE.onClick \_ -> ToggleComment
            ]
            [ HH.text "×" ]
        , HH.h4
            [ HP.style
                "margin-top: 0.5rem; margin-bottom: 1rem; margin-left: 0.5rem; font-weight: bold; color: black;"
            ]
            [ HH.text "Conversation" ]
        , HH.slot _comment unit Comment.commentview unit HandleComment
        ]
    -- CommentOverview
    , HH.div
        [ HP.classes [ HB.overflowAuto, HB.p1 ]
        , HP.style $
            "flex: 0 0 " <> show (state.sidebarRatio * 100.0)
              <>
                "%; box-sizing: border-box; min-width: 6ch; background:rgb(229, 241, 248); position: relative;"
              <>
                if
                  state.sidebarShown
                    && not state.commentShown
                    && state.commentOverviewShown then
                  ""
                else
                  "display: none;"
        ]
        [ HH.button
            [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlineSecondary ]
            , HP.style
                "position: absolute; \
                \top: 0.5rem; \
                \right: 0.5rem; \
                \background-color: #fdecea; \
                \color: #b71c1c; \
                \padding: 0.2rem 0.4rem; \
                \font-size: 0.75rem; \
                \line-height: 1; \
                \border: 1px solid #f5c6cb; \
                \border-radius: 0.2rem; \
                \z-index: 10;"
            , HE.onClick \_ -> ToggleCommentOverview false
            ]
            [ HH.text "×" ]
        , HH.h4
            [ HP.style
                "margin-top: 0.5rem; margin-bottom: 1rem; margin-left: 0.5rem; font-weight: bold; color: black;"
            ]
            [ HH.text "All comments" ]
        , HH.slot _commentOverview unit CommentOverview.commentOverviewview unit
            HandleCommentOverview
        ]
    -- Left Resizer
    , HH.div
        [ HE.onMouseDown (StartResize ResizeLeft)
        , HP.style
            "width: 8px; \
            \cursor: col-resize; \
            \background:rgba(0, 0, 0, 0.3); \
            \display: flex; \
            \align-items: center; \
            \justify-content: center; \
            \position: relative;"
        ]
        [ HH.button
            [ HP.style
                "background:rgba(255, 255, 255, 0.8); \
                \border: 0.2px solid #aaa; \
                \padding: 0.1rem 0.1rem; \
                \font-size: 8px; \
                \font-weight: bold; \
                \line-height: 1; \
                \color:rgba(0, 0, 0, 0.7); \
                \border-radius: 3px; \
                \cursor: pointer; \
                \height: 40px; \
                \width: 8px;"
            -- To prevent the resizer event under the button
            , HE.handler' (EventType "mousedown") \ev ->
                unsafePerformEffect do
                  stopPropagation ev
                  pure Nothing -- Do not trigger the mouse down event under the button
            , HE.onClick \_ -> ToggleSidebar
            ]
            [ HH.text if state.sidebarShown then "⟨" else "⟩" ]
        ]
    ]

  renderPreview :: State -> Array (H.ComponentHTML Action Slots m)
  renderPreview state =
    [ -- Right Resizer
      HH.div
        [ HE.onMouseDown (StartResize ResizeRight)
        , HP.style
            "width: 8px; \
            \cursor: col-resize; \
            \background:rgba(0, 0, 0, 0.3); \
            \display: flex; \
            \align-items: center; \
            \justify-content: center; \
            \position: relative;"
        ]
        [ HH.button
            [ HP.style
                "background:rgba(255, 255, 255, 0.8); \
                \border: 0.2px solid #aaa; \
                \padding: 0.1rem 0.1rem; \
                \font-size: 8px; \
                \font-weight: bold; \
                \line-height: 1; \
                \color:rgba(0, 0, 0, 0.7); \
                \border-radius: 3px; \
                \cursor: pointer; \
                \height: 40px; \
                \width: 8px;"
            -- To prevent the resizer event under the button
            , HE.handler' (EventType "mousedown") \ev ->
                unsafePerformEffect do
                  stopPropagation ev
                  pure Nothing -- Do not trigger the mouse down event under the button
            , HE.onClick \_ -> TogglePreview
            ]
            [ HH.text if state.previewShown then "⟩" else "⟨" ]
        ]

    -- Preview
    , if state.previewShown then
        HH.div
          [ HP.classes [ HB.dFlex, HB.flexColumn ]
          , HP.style $
              "flex: 1 1 "
                <> show (state.previewRatio * 100.0)
                <>
                  "%; box-sizing: border-box; min-height: 0; overflow: auto; min-width: 6ch; position: relative;"
          ]
          [ HH.div
              [ HP.classes [ HB.dFlex, HB.alignItemsCenter ]
              , HP.style "padding-right: 0.5rem;"
              ]
              [ HH.button
                  [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlineSecondary ]
                  , HP.style
                      "position: absolute; \
                      \top: 0.5rem; \
                      \right: 0.5rem; \
                      \background-color: #fdecea; \
                      \color: #b71c1c; \
                      \padding: 0.2rem 0.4rem; \
                      \font-size: 0.75rem; \
                      \line-height: 1; \
                      \border: 1px solid #f5c6cb; \
                      \border-radius: 0.2rem; \
                      \z-index: 10;"
                  , HE.onClick \_ -> TogglePreview
                  ]
                  [ HH.text "×" ]
              ]
          , HH.slot _preview unit Preview.preview
              { renderedHtml: state.renderedHtml }
              HandlePreview
          ]
      else
        HH.text ""
    ]

  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of

    POST -> do
      state <- H.get
      let
        tree = tocTreeToDocumentTree state.tocEntries
        encodedTree = DT.encodeDocumentTree tree

      rep <- H.liftAff $
        Request.postJson ("/docs/" <> show state.docID <> "/tree") encodedTree
      -- debugging logs in
      case rep of
        Left _ -> pure unit -- H.liftEffect $ Console.log $ Request.printError "post" err
        Right _ -> pure unit
    -- H.liftEffect $ Console.log "Successfully posted TOC to server"

    GET -> do
      s <- H.get
      -- TODO: Here, we can simply fetch the latest commit (head commit) of the document and
      --       write the content into the editor. Because requests like these are very common,
      --       we should think of a way to have a uniform and clean request handling system, especially
      --       regarding authentification and error handling. Right now, the editor page is simply empty
      --       if the document retrieval fails in any way.
      maybeTree <- H.liftAff
        $ Request.getFromJSONEndpoint DT.decodeDocument
        $ "/docs/" <> show s.docID <> "/tree/latest"
      let 
        finalTree = fromMaybe Empty (documentTreeToTOCTree <$> maybeTree)
        vMapping  = map 
          (\elem -> {elementID: elem.id, versionID: Nothing})
          finalTree
      H.modify_ _ { tocEntries = finalTree
                  , versionMapping = vMapping }
      H.tell _toc unit (TOC.ReceiveTOCs finalTree)
    Init -> do
      let timeFormatter = head timeStampsVersions
      H.modify_ \st -> do
        st { mTimeFormatter = timeFormatter }
      H.tell _comment unit (Comment.ReceiveTimeFormatter timeFormatter)
      H.tell _commentOverview unit
        (CommentOverview.ReceiveTimeFormatter timeFormatter)
      H.tell _toc unit (TOC.ReceiveTOCs Empty)
      -- Load the initial TOC entries into the editor
      -- TODO: Shoult use Get instead, but I (Eddy) don't understand GET
      -- or rather, we don't use commit anymore in the API
      handleAction GET

    -- Resizing as long as mouse is hold down on window
    -- (Or until the browser detects the mouse is released)
    StartResize which mouse -> do
      case which of
        ResizeLeft -> H.modify_ \st -> st { sidebarShown = true }
        ResizeRight -> H.modify_ \st -> st { previewShown = true }
      win <- H.liftEffect Web.HTML.window
      intWidth <- H.liftEffect $ Web.HTML.Window.innerWidth win
      let
        x = toNumber $ clientX mouse
        width = toNumber intWidth
        ratioX = x / width
      H.modify_ \st -> st
        { mDragTarget = Just which
        , startMouseRatio = ratioX
        , startSidebarRatio = st.sidebarRatio
        , startPreviewRatio = st.previewRatio
        }
      handleAction $ HandleMouseMove mouse

    -- Stop resizing, when mouse is released (is detected by browser)
    StopResize _ ->
      H.modify_ \st -> st { mDragTarget = Nothing }

    -- While mouse is hold down, resizer move to position of mouse
    -- (with certain rules)
    HandleMouseMove mouse -> do
      win <- H.liftEffect Web.HTML.window
      intWidth <- H.liftEffect $ Web.HTML.Window.innerWidth win
      let
        x = toNumber $ clientX mouse
        width = toNumber intWidth
        ratioX = x / width

        minRatio = 0.05 -- 5%
        maxRatio = 0.7 -- 70%

        clamp :: Number -> Number -> Number -> Number
        clamp minVal maxVal xval = max minVal (min maxVal xval)

      mt <- H.gets _.mDragTarget
      mx <- H.gets _.startMouseRatio

      case mt of
        Just ResizeLeft -> do
          s <- H.gets _.startSidebarRatio
          let
            rawSidebarRatio = s + (ratioX - mx)
            newSidebar = clamp minRatio 0.2 rawSidebarRatio
          when (newSidebar >= minRatio && newSidebar <= maxRatio) do
            H.modify_ \st -> st
              { sidebarRatio = newSidebar
              , lastExpandedSidebarRatio =
                  if newSidebar > minRatio then newSidebar
                  else st.lastExpandedSidebarRatio
              }

        Just ResizeRight -> do
          p <- H.gets _.startPreviewRatio
          s <- H.gets _.sidebarRatio

          let
            delta = ratioX - mx
            rawPreview = p - delta
            maxPreview = 1.0 - s - minRatio
            newPreview = clamp minRatio maxPreview rawPreview

          when (newPreview >= minRatio && newPreview <= maxPreview) do
            H.modify_ \st -> st
              { previewRatio = newPreview
              , lastExpandedPreviewRatio =
                  if newPreview > minRatio then newPreview
                  else st.lastExpandedPreviewRatio
              }

        _ -> pure unit

    -- Toggle actions

    ToggleComment -> H.modify_ \st -> st { commentShown = false }

    ToggleCommentOverview shown ->
      if shown then do
        H.tell _editor unit Editor.SendCommentSections
        H.modify_ \st -> st { commentShown = false, commentOverviewShown = shown }
      else
        H.modify_ \st -> st { commentOverviewShown = shown }

    -- Toggle the sidebar
    -- Add logic in calculating the middle ratio
    -- to restore the last expanded middle ratio, when toggling preview back on
    ToggleSidebar -> do
      state <- H.get
      -- close sidebar
      if state.sidebarShown then
        H.modify_ \st -> st
          { sidebarRatio = 0.0
          , lastExpandedSidebarRatio = st.sidebarRatio
          , sidebarShown = false
          }
      -- open sidebar
      else do
        H.modify_ \st -> st
          { sidebarRatio = st.lastExpandedSidebarRatio
          , sidebarShown = true
          }

    -- Toggle the preview area
    TogglePreview -> do
      state <- H.get
      -- all this, in order for not overlapping the left resizer (to not make it disappear)
      win <- H.liftEffect Web.HTML.window
      totalWidth <- H.liftEffect $ Web.HTML.Window.innerWidth win
      let
        w = toNumber totalWidth
        -- resizer size is 8, but there are 2 resizers.
        -- Also resizer size is not in sidebarRatio
        resizerWidth = 16.0
        resizerRatio = resizerWidth / w
      -- close preview
      if state.previewShown then do
        let
          oldPreviewRatio = state.previewRatio
        H.modify_ \st -> st
          { previewRatio = resizerRatio
          , lastExpandedPreviewRatio = oldPreviewRatio
          , previewShown = false
          }
      -- open preview
      else do
        -- restore the last expanded middle ratio, when toggling preview back on
        H.modify_ \st -> st
          { previewRatio = st.lastExpandedPreviewRatio
          , previewShown = true
          }

    ModifyVersionMapping tocID vID -> do
      state <- H.get
      let
        newVersionMapping = 
          modifyNodeRootTree
            (\v -> v.elementID == tocID)
            (\string -> string)
            (\v -> {elementID: v.elementID, versionID: vID})
            state.versionMapping
      H.modify_ _ {versionMapping = newVersionMapping}

    -- Query handler

    HandleComment output -> case output of

      Comment.CloseCommentSection -> do
        H.modify_ \st -> st { commentShown = false }

      -- behaviour for old versions still to discuss. for now will simply fail if old element version selected.
      Comment.UpdateComment tocID markerID newCommentSection -> do
        H.tell _editor unit Editor.SaveSection
        state <- H.get
        case findRootTree (\e -> e.elementID == tocID && e.versionID /= Nothing) state.versionMapping of
          Just _ -> do
            let
              updatedTOCEntries = map
                ( \entry ->
                    if entry.id /= tocID then entry
                    else
                      let
                        newMarkers =
                          ( map
                              ( \marker ->
                                  if marker.id /= markerID then marker
                                  else marker { mCommentSection = Just newCommentSection }
                              )
                              entry.markers
                          )
                      in
                        entry { markers = newMarkers }
                )
                state.tocEntries
              updateTOCEntry = fromMaybe
                emptyTOCEntry
                (findTOCEntry tocID updatedTOCEntries)
              title = fromMaybe
                ""
                (findTitleTOCEntry tocID updatedTOCEntries)
            H.modify_ \s -> s { tocEntries = updatedTOCEntries }
            H.tell _editor unit (Editor.ChangeSection title updateTOCEntry Nothing)
          Nothing -> do
            H.liftEffect $ log "unable to unpdate comment on outdated versions of elements"

    HandleCommentOverview output -> case output of

      CommentOverview.JumpToCommentSection tocID markerID commentSection -> do
        H.modify_ \st -> st { commentShown = true }
        H.tell _comment unit
          (Comment.SelectedCommentSection tocID markerID commentSection)

    HandleEditor output -> case output of

      Editor.ClickedQuery response -> do
        renderedHtml' <- H.liftAff $ Request.postRenderHtml (joinWith "\n" response)
        case renderedHtml' of
          Left _ -> pure unit -- Handle error
          Right { body } -> do
            H.modify_ \st -> st { renderedHtml = Just body }

      Editor.DeletedComment tocEntry deletedIDs -> do
        H.modify_ \st ->
          st
            { tocEntries =
                map (\e -> if e.id == tocEntry.id then tocEntry else e) st.tocEntries
            }
        H.tell _comment unit (Comment.DeletedComment tocEntry.id deletedIDs)

      Editor.SavedSection toBePosted title tocEntry -> do
        state <- H.get
        let
          newTOCTree = replaceTOCEntry tocEntry.id title tocEntry state.tocEntries
        H.modify_ \st -> st { tocEntries = newTOCTree }
        H.tell _toc unit (TOC.ReceiveTOCs newTOCTree)
        when toBePosted (handleAction POST)

      Editor.SelectedCommentSection tocID markerID -> do
        state <- H.get
        if state.sidebarShown then
          H.modify_ \st -> st { commentShown = true }
        else
          H.modify_ \st -> st
            { sidebarRatio = st.lastExpandedSidebarRatio
            , sidebarShown = true
            , commentShown = true
            }
        case (findCommentSection state.tocEntries tocID markerID) of
          Nothing -> pure unit
          Just commentSection -> do
            H.tell _comment unit
              (Comment.SelectedCommentSection tocID markerID commentSection)

      Editor.SendingTOC tocEntry -> do
        H.tell _commentOverview unit (CommentOverview.ReceiveTOC tocEntry)

      Editor.ShowAllCommentsOutput -> handleAction $ ToggleCommentOverview true
    HandlePreview _ -> pure unit

    HandleTOC output -> case output of

      TOC.ModifyVersion elementID mVID -> do
        H.liftEffect $ log $ show mVID
        handleAction (ModifyVersionMapping elementID mVID) 

      TOC.ChangeSection title selectedId -> do
        H.tell _editor unit Editor.SaveSection
        state <- H.get
        let
          entry = case (findTOCEntry selectedId state.tocEntries) of
            Nothing -> emptyTOCEntry
            Just e -> e
          rev = case findRootTree (\e -> e.elementID == selectedId) state.versionMapping of
            Nothing -> Nothing
            Just elem -> elem.versionID
        -- handleAction (ModifyVersionMapping selectedID rev)
        H.tell _editor unit (Editor.ChangeSection title entry rev)

      TOC.AddNode path node -> do
        state <- H.get
        let
          newTree = addRootNode path node state.tocEntries
          docTree = tocTreeToDocumentTree newTree
          encodeTree = DT.encodeDocumentTree docTree
        _ <- H.liftAff $
          Request.postJson ("/docs/" <> show state.docID <> "/tree") encodeTree
        H.modify_ \st -> st { tocEntries = newTree }
        H.tell _toc unit (TOC.ReceiveTOCs newTree)

      TOC.DeleteNode path -> do
        -- TODO: Lots of code taken from `HandleTOC TOC.AddNode`, need to refactor! 
        state <- H.get
        let
          newTree = deleteRootNode path state.tocEntries
          docTree = tocTreeToDocumentTree newTree
          encodeTree = DT.encodeDocumentTree docTree
        _ <- H.liftAff $
          Request.postJson ("/docs/" <> show state.docID <> "/tree") encodeTree
        H.modify_ \st -> st { tocEntries = newTree }
        H.tell _toc unit (TOC.ReceiveTOCs newTree)

      TOC.ReorderItems { from, to } -> do
        -- TODO: Lots of code taken from `HandleTOC TOC.AddNode`, need to refactor! 
        state <- H.get
        let
          newTree = reorderTocEntries from to state.tocEntries
          docTree = tocTreeToDocumentTree newTree
          encodeTree = DT.encodeDocumentTree docTree
        _ <- H.liftAff $
          Request.postJson ("/docs/" <> show state.docID <> "/tree") encodeTree
        H.modify_ \st -> st { tocEntries = newTree }
        H.tell _toc unit (TOC.ReceiveTOCs newTree)

findCommentSection :: TOCTree -> Int -> Int -> Maybe CommentSection
findCommentSection tocEntries tocID markerID = do
  tocEntry <- findTOCEntry tocID tocEntries
  marker <- find (\m -> m.id == markerID) tocEntry.markers
  marker.mCommentSection

-- Add a node in TOC tree
addRootNode
  :: Array Int
  -> Tree TOCEntry
  -> TOCTree
  -> TOCTree
addRootNode [] entry (RootTree { children, header }) =
  RootTree { children: snoc children (Edge entry), header }
addRootNode _ entry Empty =
  RootTree
    { children: [ Edge entry ], header: { headerKind: "root", headerType: "root" } }
addRootNode path entry (RootTree { children, header }) =
  case uncons path of
    Nothing ->
      RootTree { children: snoc children (Edge entry), header }
    Just { head, tail } ->
      let
        child =
          fromMaybe
            (Edge (Leaf { title: "Error", node: emptyTOCEntry }))
            (children !! head)
        newChildren =
          case updateAt head (addNode tail entry child) children of
            Nothing -> children
            Just res -> res
      in
        RootTree { children: newChildren, header }

addNode
  :: Array Int
  -> Tree TOCEntry
  -> Edge TOCEntry
  -> Edge TOCEntry
addNode _ _ (Edge (Leaf { title, node })) =
  Edge (Leaf { title, node }) -- Cannot add to a leaf
addNode [] entry (Edge (Node { title, children, header })) =
  Edge (Node { title, children: snoc children (Edge entry), header })
addNode path entry (Edge (Node { title, children, header })) =
  case uncons path of
    Nothing ->
      Edge (Node { title, children: snoc children (Edge entry), header })
    Just { head, tail } ->
      let
        child =
          fromMaybe
            (Edge (Leaf { title: "Error", node: emptyTOCEntry }))
            (children !! head)
        newChildren' =
          case updateAt head (addNode tail entry child) children of
            Nothing -> children
            Just res -> res
      in
        Edge (Node { title, children: newChildren', header })

deleteRootNode
  :: Array Int
  -> TOCTree
  -> TOCTree
deleteRootNode _ Empty = Empty
deleteRootNode [] _ = Empty
deleteRootNode path (RootTree { children, header }) =
  case uncons path of
    Nothing ->
      RootTree { children, header } -- no path, do nothing
    Just { head, tail } ->
      if null tail then
        -- Delete the child at index `head`
        case deleteAt head children of
          Nothing -> RootTree { children, header }
          Just newChildren -> RootTree { children: newChildren, header }
      else
        let
          child =
            fromMaybe
              (Edge (Leaf { title: "Error", node: emptyTOCEntry }))
              (children !! head)
          newChildren =
            case updateAt head (deleteNode tail child) children of
              Nothing -> children
              Just res -> res
        in
          RootTree { children: newChildren, header }

deleteNode
  :: Array Int
  -> Edge TOCEntry
  -> Edge TOCEntry
deleteNode _ edge@(Edge (Leaf _)) =
  edge -- Cannot delete deeper inside a leaf
deleteNode [] e =
  -- If path is empty, delete this node entirely is handled by parent
  -- so this case should not normally be reached.
  e
deleteNode path (Edge (Node { title, children, header })) =
  case uncons path of
    Nothing ->
      Edge (Node { title, children, header })
    Just { head, tail } ->
      if null tail then
        case deleteAt head children of
          Nothing -> Edge (Node { title, children, header })
          Just newChildren -> Edge (Node { title, children: newChildren, header })
      else
        let
          child =
            fromMaybe
              (Edge (Leaf { title: "Error", node: emptyTOCEntry }))
              (children !! head)
          newChildren' =
            case updateAt head (deleteNode tail child) children of
              Nothing -> children
              Just res -> res
        in
          Edge (Node { title, children: newChildren', header })

-- Reorder TOC entries by moving a node from `sourcePath` to `targetPath`.
-- The node at sourcePath takes the place of the node at targetPath,
-- and everything shifts accordingly.
reorderTocEntries :: Array Int -> Array Int -> TOCTree -> TOCTree
reorderTocEntries sourcePath targetPath tree
  | sourcePath == targetPath = tree
  | otherwise = case extractNodeAtPath sourcePath tree of
      Nothing -> tree
      Just extractedNode ->
        let
          treeWithoutSource = deleteRootNode sourcePath tree
          adjustedTargetPath = adjustPathAfterDeletion sourcePath targetPath
        in
          insertNodeAtPosition adjustedTargetPath extractedNode treeWithoutSource

-- Adjust target path after source deletion to account for index shifts
adjustPathAfterDeletion :: Array Int -> Array Int -> Array Int
adjustPathAfterDeletion sourcePath targetPath =
  adjustPathRecursive sourcePath targetPath

adjustPathRecursive :: Array Int -> Array Int -> Array Int
adjustPathRecursive sourcePath targetPath =
  case uncons sourcePath, uncons targetPath of
    Just { head: srcHead, tail: srcTail }, Just { head: tgtHead, tail: tgtTail } ->
      if null srcTail && null tgtTail then
        -- Both are at the same level (siblings)
        if tgtHead > srcHead then
          -- Target is after source, so decrement target index since source was removed
          [ tgtHead - 1 ]
        else
          -- Target is before or at source position, stays the same
          targetPath
      else if srcHead == tgtHead then
        -- Same parent, continue recursively
        cons tgtHead (adjustPathRecursive srcTail tgtTail)
      else
        -- Different branches at this level
        if null srcTail then
        -- Source is being deleted at this level
        if tgtHead > srcHead then
          cons (tgtHead - 1) tgtTail
        else
          targetPath
      else
        -- Source deletion is deeper, no adjustment needed
        targetPath
    _, _ -> targetPath

-- Extract a node at a given path without deleting it
extractNodeAtPath :: Array Int -> TOCTree -> Maybe (Tree TOCEntry)
extractNodeAtPath _ Empty = Nothing
extractNodeAtPath [] _ = Nothing -- Cannot extract root
extractNodeAtPath path (RootTree { children }) =
  case uncons path of
    Nothing -> Nothing
    Just { head, tail } ->
      case children !! head of
        Nothing -> Nothing
        Just (Edge node) ->
          if null tail then
            Just node
          else
            extractNodeFromTree tail node

extractNodeFromTree :: Array Int -> Tree TOCEntry -> Maybe (Tree TOCEntry)
extractNodeFromTree _ (Leaf _) = Nothing -- Cannot go deeper in leaf
extractNodeFromTree [] node = Just node
extractNodeFromTree path (Node { children }) =
  case uncons path of
    Nothing -> Nothing
    Just { head, tail } ->
      case children !! head of
        Nothing -> Nothing
        Just (Edge node) ->
          if null tail then
            Just node
          else
            extractNodeFromTree tail node

-- Insert node at the exact target position (pushing existing nodes down)
insertNodeAtPosition :: Array Int -> Tree TOCEntry -> TOCTree -> TOCTree
insertNodeAtPosition [] node tree =
  -- Insert at root level (append to end)
  case tree of
    Empty -> RootTree
      { children: [ Edge node ]
      , header: { headerKind: "root", headerType: "root" }
      }
    RootTree { children, header } ->
      RootTree { children: snoc children (Edge node), header }

insertNodeAtPosition _ node Empty =
  RootTree
    { children: [ Edge node ]
    , header: { headerKind: "root", headerType: "root" }
    }

insertNodeAtPosition path node (RootTree { children, header }) =
  case uncons path of
    Nothing -> RootTree { children: snoc children (Edge node), header }
    Just { head, tail } ->
      if null tail then
        -- Insert exactly at position `head`, pushing existing elements down
        case insertAt head (Edge node) children of
          Nothing ->
            -- If insertion fails (index out of bounds), append to end
            RootTree { children: snoc children (Edge node), header }
          Just result ->
            RootTree { children: result, header }
      else
        -- Navigate deeper into the tree
        case children !! head of
          Nothing ->
            -- Path doesn't exist, cannot insert deeper
            RootTree { children, header }
          Just childEdge ->
            let
              newChild = insertNodeIntoEdgeAtPosition tail node childEdge
              newChildren = case updateAt head newChild children of
                Nothing -> children
                Just res -> res
            in
              RootTree { children: newChildren, header }

insertNodeIntoEdgeAtPosition
  :: Array Int -> Tree TOCEntry -> Edge TOCEntry -> Edge TOCEntry
insertNodeIntoEdgeAtPosition _ _ edge@(Edge (Leaf _)) = edge -- Cannot insert into leaf
insertNodeIntoEdgeAtPosition [] node (Edge (Node { title, children, header })) =
  -- Insert at end of children
  Edge (Node { title, children: snoc children (Edge node), header })
insertNodeIntoEdgeAtPosition path node (Edge (Node { title, children, header })) =
  case uncons path of
    Nothing -> Edge (Node { title, children: snoc children (Edge node), header })
    Just { head, tail } ->
      if null tail then
        -- Insert exactly at position `head`, pushing existing elements down
        case insertAt head (Edge node) children of
          Nothing ->
            -- If insertion fails (index out of bounds), append to end
            Edge (Node { title, children: snoc children (Edge node), header })
          Just result ->
            Edge (Node { title, children: result, header })
      else
        -- Navigate deeper
        case children !! head of
          Nothing -> Edge (Node { title, children, header })
          Just childEdge ->
            let
              newChild = insertNodeIntoEdgeAtPosition tail node childEdge
              newChildren = case updateAt head newChild children of
                Nothing -> children
                Just res -> res
            in
              Edge (Node { title, children: newChildren, header })
