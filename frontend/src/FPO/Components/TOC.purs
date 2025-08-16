
module FPO.Components.TOC where

import Prelude

import Data.Array (concat, mapWithIndex)
import Data.DateTime
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Effect.Now (nowDateTime)
import FPO.Components.Modals.DeleteModal (deleteConfirmationModal)
import FPO.Data.Request as Request
import FPO.Data.Store as Store
import FPO.Dto.DocumentDto.DocDate as DD
import FPO.Dto.DocumentDto.DocumentHeader as DH
import FPO.Dto.DocumentDto.TextElement as TE
import FPO.Dto.DocumentDto.TreeDto (Edge(..), RootTree(..), Tree(..))
import FPO.Dto.PostTextDto (PostTextDto(..))
import FPO.Dto.PostTextDto as PostTextDto
import FPO.Page.Home (formatRelativeTime)
import FPO.Translations.Translator (fromFpoTranslator)
import FPO.Translations.Util (FPOState)
import FPO.Types (ShortendTOCEntry, TOCEntry, TOCTree, shortenTOC)
import FPO.Util (prependIf)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Halogen.Themes.Bootstrap5 as HB
import Web.Event.Event (preventDefault)
import Web.HTML.Event.DragEvent (DragEvent, toEvent)

type Input = DH.DocumentID

type Version = {identifier :: Int, timestamp :: DD.DocDate}

data Output
  = ChangeSection String Int
  | AddNode Path (Tree TOCEntry)
  | DeleteNode Path
  | ReorderItems { from :: Path, to :: Path }
  | ModifyVersion Int (Maybe Int)

type Path = Array Int

data Action
  = Init
  | Both Action Action
  | Receive (Connected Store.Store Input)
  | JumpToSection String Int
  | ToggleAddMenu (Array Int)
  | ToggleHistoryMenu (Array Int) Int
  | ToggleHistorySubmenu
  | CreateNewSubsection (Array Int)
  | CreateNewSection (Array Int)
  | OpenVersion Int Int
  | CompareVersion Int Int
  | UpdateVersions DateTime Int
  -- | Section deletion
  | RequestDeleteSection Path
  | CancelDeleteSection
  | ConfirmDeleteSection Path
  -- | Drag and Drop
  | StartDrag Path
  | HighlightDropZone Path DropPosition DragEvent
  | ClearDropZones
  | CompleteDrop Path

-- TODO: This is not used yet, but we will need it (or something similar)
--       for full and robust `Drag and Drop` support.
data DropPosition
  = Before
  | After

data Query a = ReceiveTOCs (TOCTree) a

type State = FPOState
  ( docID :: DH.DocumentID
  , documentName :: String
  , tocEntries :: RootTree ShortendTOCEntry
  , mSelectedTocEntry :: Maybe Int
  , now :: Maybe DateTime
  , showAddMenu :: Array Int
  , showHistoryMenu :: Array Int
  , showHistorySubmenu :: Boolean
  , versions :: Array Version
  , dragState :: Maybe { draggedId :: Path, hoveredId :: Path }
  , requestDelete :: Maybe Path
  )

tocview
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => H.Component Query Input Output m
tocview = connect (selectEq identity) $ H.mkComponent
  { initialState: \{ context: store, input } ->
      { documentName: ""
      , tocEntries: Empty
      , mSelectedTocEntry: Nothing
      , now: Nothing
      , showAddMenu: [ -1 ]
      , showHistoryMenu: [ -1 ]
      , showHistorySubmenu: false
      , versions: []
      , docID: input
      , dragState: Nothing
      , requestDelete: Nothing
      , translator: fromFpoTranslator store.translator
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      , receive = Just <<< Receive
      , handleQuery = handleQuery
      }
  }
  where

  render :: State -> forall slots. H.ComponentHTML Action slots m
  render state =
    HH.div_ $
      renderDeleteModal
        <>
          ( rootTreeToHTML 
              state 
              state.documentName 
              state.showAddMenu
              state.showHistoryMenu
              state.mSelectedTocEntry
              state.now
              state.tocEntries
          )
    where
    renderDeleteModal = case state.requestDelete of
      Nothing -> []
      Just path ->
        [ deleteConfirmationModal
            state.translator
            path
            (\p -> "Section " <> show p)
            CancelDeleteSection
            ConfirmDeleteSection
            ""
        ]

  handleAction :: Action -> forall slots. H.HalogenM State Action slots Output m Unit
  handleAction = case _ of
    Init -> do
      s <- H.get
      now <- liftEffect nowDateTime
      mDoc <- H.liftAff $ Request.getDocumentHeader s.docID
      let
        docName = case mDoc of
          Nothing -> ""
          Just doc -> DH.getName doc
      H.modify_ \st -> do
        st { documentName = docName
           , now = Just now }

    Both act1 act2 -> do
      handleAction act1
      handleAction act2

    UpdateVersions ts elementID -> do
      s <- H.get
      history <- H.liftAff $ Request.getTextElemHistory s.docID elementID (DD.DocDate ts) 5
      case history of
        Nothing -> do liftEffect $ log "unable to load textElements"
        Just h -> do
          let
            newVersions = map 
              (\hEntry -> {identifier: TE.getHistoryElementID hEntry, timestamp: TE.getHistoryElementTimestamp hEntry})
              (TE.getTEHsFromFTEH h)
          H.modify_ _ { versions = newVersions}

    OpenVersion elementID vID-> do
      H.raise (ModifyVersion elementID (Just vID))

    CompareVersion elementID vID -> do
      pure unit

{-     JumpToSection title id -> do
      H.modify_ \state ->
        state { mSelectedTocEntry = Just id }
      H.raise (ChangeSection title id) -}
    JumpToSection title id -> do
      H.modify_ \state ->
        state { mSelectedTocEntry = Just id }
      H.raise (ChangeSection title id)

    ToggleAddMenu path -> do
      H.modify_ \state ->
        state
          { showAddMenu =
              if state.showAddMenu == [ -1 ] || state.showAddMenu /= path then path
              else [ -1 ]
          }
    
    ToggleHistoryMenu path elementID -> do
      H.modify_ \state ->
        state
          { showHistoryMenu =
              if state.showHistoryMenu == [ -1 ] || state.showHistoryMenu /= path then path
              else [ -1 ]
          }
      -- liftEffect $ log "Toggled History Menu"
      now <- liftEffect nowDateTime   
      handleAction (UpdateVersions now elementID)

    ToggleHistorySubmenu -> do
      H.modify_ \state -> state { showHistorySubmenu = (false == state.showHistorySubmenu) }

    CreateNewSubsection path -> do
      H.modify_ _ { showAddMenu = [ -1 ] }
      s <- H.get
      gotRes <- H.liftAff $
        Request.postJson ("/docs/" <> show s.docID <> "/text")
          ( PostTextDto.encodePostTextDto
              (PostTextDto { identifier: 0, kind: "new Text" })
          )
      case gotRes of
        Left _ -> pure unit
        Right res -> do
          let
            textDto = PostTextDto.decodePostTextDto res.body
          case textDto of
            Left _ -> pure unit
            Right dto -> do
              let
                newEntry =
                  Leaf
                    { title: "New Subsection"
                    , node:
                        { id: PostTextDto.getID dto
                        , name: "New Subsection"
                        , paraID: 0 -- to be implemented later
                        , newMarkerNextID: 0
                        , markers: []
                        }
                    }
              H.raise (AddNode path newEntry)

    CreateNewSection path -> do
      H.modify_ \st ->
        st { showAddMenu = [ -1 ] }
      let
        newEntry = Node
          { title: "New Section"
          , children: []
          , header: { headerKind: "section", headerType: "section" }
          }
      H.raise (AddNode path newEntry)

    RequestDeleteSection path -> do
      liftEffect $ log $ "TODO: DeleteSection at path: " <> show path
      H.modify_ _ { requestDelete = Just path }

    CancelDeleteSection -> do
      H.modify_ _ { requestDelete = Nothing }

    ConfirmDeleteSection path -> do
      liftEffect $ log $ "TODO: ConfirmDeleteSection at path: " <> show path
      H.raise (DeleteNode path)
      H.modify_ _ { requestDelete = Nothing }

    StartDrag id -> do
      H.modify_ _ { dragState = Just { draggedId: id, hoveredId: id } }

    -- TODO: position. As of now, we only support drop zones *before* an item. For `n` items,
    --       this amounts to `n` drop zones. But we need `n+1` drop zones, one for each item and
    --       one at the end. Not sure how to handle this properly, but we could easily just
    --       add a drop zone at the very end, with a position of `After`.
    HighlightDropZone targetId _ e -> do
      -- We need to prevent the default behavior to allow dropping.
      H.liftEffect $ preventDefault (toEvent e)
      H.modify_ \s -> s { dragState = map (_ { hoveredId = targetId }) s.dragState }

    ClearDropZones -> do
      liftEffect $ log "ClearDropZones"
      H.modify_ _ { dragState = Nothing }

    CompleteDrop targetId -> do
      liftEffect $ log ("CompleteDrop: " <> show targetId)
      state <- H.get
      case state.dragState of
        Just { draggedId } -> do
          H.raise (ReorderItems { from: draggedId, to: targetId })
          liftEffect $ log
            ( "CompleteDrop: draggedId = " <> show draggedId <> ", hoveredId = " <>
                show targetId
            )
          handleAction ClearDropZones
        Nothing -> pure unit

    Receive { context: store } -> do
      H.modify_ _
        { translator = fromFpoTranslator store.translator
        }

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of
    ReceiveTOCs entries a -> do
      let
        shortendEntries = map shortenTOC entries
      H.modify_ \state ->
        state
          { tocEntries = shortendEntries }
      pure (Just a)

  rootTreeToHTML
    :: forall slots
     . State
    -> String
    -> Array Int
    -> Array Int
    -> Maybe Int
    -> Maybe DateTime
    -> RootTree ShortendTOCEntry
    -> Array (H.ComponentHTML Action slots m)
  rootTreeToHTML _ _ _ _ _ _ Empty = []
  rootTreeToHTML state docName menuPath historyPath mSelectedTocEntry now (RootTree { children }) =
    [ HH.div
        [ HP.classes [ HB.bgWhite ] ]
        [ HH.div
            [ HP.classes [ HB.borderBottom, HB.ms1, HB.me2 ] ]
            [ HH.div
                [ HP.classes
                    [ HB.dFlex, HB.alignItemsCenter, HB.justifyContentBetween ]
                ]
                [ HH.span
                    [ HP.classes [ HB.fwSemibold, HB.textTruncate, HB.fs4, HB.p2 ] ]
                    [ HH.text docName ]
                , renderButtonInterface menuPath historyPath [] false
                ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "toc-list" ] ]
            ( concat $ mapWithIndex
                ( \ix (Edge child) ->
                    treeToHTML state menuPath historyPath 1 mSelectedTocEntry [ ix ] now child
                )
                children
            )
        ]
    ]

  -- TODO: There's still a lot of duplicate code between `Node` and `Leaf` cases.
  treeToHTML
    :: forall slots
     . State
    -> Array Int
    -> Array Int
    -> Int
    -> Maybe Int
    -> Array Int
    -> Maybe DateTime
    -> Tree ShortendTOCEntry
    -> Array (H.ComponentHTML Action slots m)
  treeToHTML state menuPath historyPath level mSelectedTocEntry path now = case _ of
    Node { title, children } ->
      let
        innerDivClasses =
          [ HB.dFlex, HB.alignItemsCenter, HB.py2, HB.positionRelative ]
        titleClasses =
          [ HB.textTruncate, HB.flexGrow1, HB.fwBold, HB.fs5 ]
      in
        [ HH.div
            ([ HP.classes $ [ HH.ClassName "toc-item", HB.rounded ] ] <> dragProps)
            [ addDropZone state path
            , HH.div
                [ HP.classes innerDivClasses ]
                [ dragHandle
                , HH.span
                    [ HP.classes titleClasses
                    , HP.style "align-self: stretch; flex-basis: 0;"
                    ]
                    [ HH.text title ]
                , renderButtonInterface menuPath historyPath path true
                ]
            ]
        ] <> concat
          ( mapWithIndex
              ( \ix (Edge child) ->
                  treeToHTML state menuPath historyPath (level + 1) mSelectedTocEntry
                    (path <> [ ix ])
                    now
                    child
              )
              children
          )

    Leaf { title, node: { id, paraID: _, name: _ } } ->
      let
        selectedClasses =
          if Just id == mSelectedTocEntry then
            [ HB.bgPrimary, HH.ClassName "bg-opacity-10", HB.textPrimary ]
          else []
        containerProps =
          ( [ HP.classes $ [ HH.ClassName "toc-item", HB.rounded ] <> selectedClasses
            , HP.title ("Jump to section " <> title)
            ] <> dragProps
          )
        innerDivBaseClasses =
          [ HB.dFlex, HB.alignItemsCenter, HB.py2, HB.positionRelative ]
        innerDivProps =
          [ HP.classes innerDivBaseClasses
          , HP.style "cursor: pointer;"
          ] <>
            (if level > 0 then [ HE.onClick \_ -> JumpToSection title id ] else [])
      in
        [ HH.div
            containerProps
            [ addDropZone state path
            , HH.div
                innerDivProps
                [ dragHandle
                , HH.span
                    [ HP.classes
                        [ HB.textTruncate, HB.flexGrow1, HB.fwNormal, HB.fs6 ]
                    , HP.style "align-self: stretch; flex-basis: 0;"
                    ]
                    [ HH.text title ]
                , HH.div [ HP.classes [ HB.positionRelative ] ]
                    [ deleteSectionButton path
                    , versionHistoryButton historyPath path state.versions state.showHistorySubmenu now id
                    ]
                ]
            ]
        ]
    where
    dragProps =
      [ HP.draggable true
      , HE.onDragStart $ const $ StartDrag path
      , HE.onDragOver $ HighlightDropZone path Before
      , HE.onDrop $ const $ CompleteDrop path
      , HE.onDragEnd $ const $ ClearDropZones
      ]

    dragHandle = HH.span
      [ HP.classes
          [ HH.ClassName "toc-drag-handle", HB.textMuted, HB.me2 ]
      , HP.style ("margin-left: " <> show level <> "rem;")
      ]
      [ HH.text "⋮⋮" ]

  -- Helper to check if the current path is the active dropzone.
  -- This is used to highlight the dropzone when dragging an item.
  activeDropzone
    :: State -> Array Int -> Boolean
  activeDropzone state path =
    case state.dragState of
      Just { draggedId, hoveredId } ->
        hoveredId == path && hoveredId /= draggedId
      _ -> false

  -- Creates a drop zone for the current path.
  addDropZone
    :: forall slots. State -> Array Int -> H.ComponentHTML Action slots m
  addDropZone state path = HH.div
    [ HP.classes
        $ prependIf (activeDropzone state path) (H.ClassName "active")
        $ [ H.ClassName "drop-zone" ]
    ]
    []


  -- Creates a delete button for the section.
  deleteSectionButton
    :: forall slots. Array Int -> H.ComponentHTML Action slots m
  deleteSectionButton path =
    HH.button
      [ HP.classes
          [ HB.btn
          , HB.btnDanger
          , HH.ClassName "toc-button"
          , HH.ClassName "toc-add-wrapper"
          ]
      , HE.onClick $ const $ RequestDeleteSection path
      ]
      [ HH.text "-" ]

  {- versionHistoryButton
    :: forall slots. Path -> Path -> Array Version -> Boolean -> Maybe DateTime -> Int -> H.ComponentHTML Action slots m
  versionHistoryButton historyPath path versions showHistorySubmenu now elementID = 
    HH.button
      [ HP.classes
          [ HB.btn
          , HB.btnSecondary
          , HH.ClassName "toc-button"
          , HH.ClassName "toc-add-wrapper"
          , H.ClassName "bi bi-clock-history"
          ] 
      , HE.onClick $ const $ ToggleHistoryMenu path elementID
      ]
      [ if historyPath == path then
          HH.div
            [ HP.classes
                [ HB.positionAbsolute
                , HB.bgWhite  
                , HB.border
                , HB.rounded
                , HB.shadowSm
                , HB.py1
                ]
            , HP.style "top: 100%; right: 0; z-index: 1000; min-width: 160px;"
            ]
            (versionHistoryMenu versions showHistorySubmenu)
        else
            HH.text ""
          ] -}
  versionHistoryButton
    :: forall slots. Path -> Path -> Array Version -> Boolean -> Maybe DateTime -> Int -> H.ComponentHTML Action slots m
  versionHistoryButton historyPath path versions showHistorySubmenu now elementID = 
    HH.div
      [ HP.classes [ HB.positionRelative ] ] $
      [HH.button
        [ HP.classes
            [ HB.btn
            , HB.btnSecondary
            , HH.ClassName "toc-button"
            , HH.ClassName "toc-add-wrapper"
            , H.ClassName "bi bi-clock-history"
            ] 
        , HE.onClick $ const $ ToggleHistoryMenu path elementID
        ]
        []
      ]
        <>
      [ if historyPath == path then
          HH.div
            [ HP.classes
                [ HB.positionAbsolute
                , HB.bgWhite  
                , HB.border
                , HB.rounded
                , HB.shadowSm
                , HB.py1
                ]
            , HP.style "top: 100%; right: 0; z-index: 1000; min-width: 160px;"
            ]
            (versionHistoryMenu versions showHistorySubmenu)
        else
            HH.text ""
          ]
    where 
    -- this is a placeholder that only allows to look at the 5 last versions
    -- versionHistoryMenu :: forall slots. Array Version -> Boolean -> Array (H.ComponentHTML Action slots m)
    versionHistoryMenu versions showHistorySubmenu = 
      map 
        (\v -> addVersionButton v now)
        versions

    -- addVersionButton :: forall slots. Version -> (H.ComponentHTML Action slots m)
    addVersionButton version now = 
      HH.button 
        [ HP.classes
            [ HB.btn
            , HB.btnLink
            , HB.textStart
            , HB.textDecorationNone
            , HB.w100
            , HB.border0
            , HB.textBody
            , HB.dFlex
            , HB.alignItemsCenter
            ]
        , HE.onClick \_ -> ToggleHistorySubmenu
        ] $
        [ HH.div [ HP.classes [ H.ClassName "bi bi-clock-history", HB.fs5, HB.me1 ] ] []
        , HH.div [ HP.classes [ HB.fs6 ] ]
            [ HH.text (formatRelativeTime now (DD.docDateToDateTime version.timestamp)) ]
        ]
          <>
            [ if showHistorySubmenu then
                HH.div
                  [ HP.classes
                      [ HB.positionAbsolute
                      , HB.bgWhite
                      , HB.border
                      , HB.rounded
                      , HB.shadowSm
                      , HB.py1
                      ]
                  , HP.style "top: 100%; right: 0; z-index: 1000; min-width: 160px;"
                  ]
                  [ versionHistorySubmenuButton "view Version" OpenVersion version
                  , versionHistorySubmenuButton "Compare to Current Version" CompareVersion version
                  ]
              else
                  HH.text ""
              ]

    versionHistorySubmenuButton title act version =
      HH.button
        [ HP.classes
            [ HB.btn
            , HB.btnLink
            , HB.textStart
            , HB.textDecorationNone
            , HB.w100
            , HB.border0
            , HB.textBody
            , HB.dFlex
            , HB.alignItemsCenter
            ]
        , HE.onClick \_ -> Both (act elementID version.identifier) (ToggleHistoryMenu path elementID)
        ]
        [ HH.div [ HP.classes [ HB.fs6 ] ]
            [ HH.text title ]
        ]


{-   versionHistoryButton path = 
    HH.button
      [ HP.classes
          [ HB.btn
          , HB.btnSecondary
          , HH.ClassName "toc-button"
          , HH.ClassName "toc-add-wrapper"
          ] 
      , HE.onClick $ const $ RequestDeleteSection path
      ]
      [ HH.div [ HP.classes [ H.ClassName "bi bi-clock-history", HB.fs5, HB.me1 ] ] [] ] -}


  -- Helper to render add button with dropdown, and optional delete button.
  renderButtonInterface
    :: forall slots
     . Array Int
    -> Array Int
    -> Array Int
    -> Boolean
    -> H.ComponentHTML Action slots m
  renderButtonInterface menuPath historyPath currentPath renderDeleteBtn =
    HH.div
      [ HP.classes [ HB.positionRelative ] ] $
      [ HH.button
          [ HP.classes
              [ HB.btn
              , HB.btnSuccess
              , HH.ClassName "toc-button"
              , HH.ClassName "toc-add-wrapper"
              ]
          , HE.onClick \_ -> ToggleAddMenu currentPath
          ]
          [ HH.text "+" ]
      ]
        <>
          ( if renderDeleteBtn then [ deleteSectionButton currentPath ]
            else []
          )
        <>
          [ if menuPath == currentPath then
              HH.div
                [ HP.classes
                    [ HB.positionAbsolute
                    , HB.bgWhite
                    , HB.border
                    , HB.rounded
                    , HB.shadowSm
                    , HB.py1
                    ]
                , HP.style "top: 100%; right: 0; z-index: 1000; min-width: 160px;"
                ]
                [ addSectionButton "Unterabschnitt" CreateNewSubsection
                , addSectionButton "Abschnitt" CreateNewSection
                ]
            else
              HH.text ""
          ]
    where
    addSectionButton str act = HH.button
      [ HP.classes
          [ HB.btn
          , HB.btnLink
          , HB.textStart
          , HB.textDecorationNone
          , HB.w100
          , HB.border0
          , HB.textBody
          , HB.dFlex
          , HB.alignItemsCenter
          ]
      , HE.onClick \_ -> act currentPath
      ]
      [ HH.div [ HP.classes [ H.ClassName "bi bi-plus", HB.fs5, HB.me1 ] ] []
      , HH.div [ HP.classes [ HB.fs6 ] ]
          [ HH.text str ]
      ]
