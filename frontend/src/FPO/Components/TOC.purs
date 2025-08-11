module FPO.Components.TOC where

import Prelude

import Data.Array (concat, mapWithIndex)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Data.Request as Request
import FPO.Data.Store as Store
import FPO.Dto.DocumentDto.DocumentHeader as DH
import FPO.Dto.DocumentDto.TreeDto (Edge(..), RootTree(..), Tree(..))
import FPO.Dto.PostTextDto (PostTextDto(..))
import FPO.Dto.PostTextDto as PostTextDto
import FPO.Types (ShortendTOCEntry, TOCEntry, TOCTree, shortenTOC)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB

type Input = DH.DocumentID

data Output
  = ChangeSection String Int
  | AddNode (Array Int) (Tree TOCEntry)

data Action
  = Init
  | JumpToSection String Int
  | ToggleAddMenu (Array Int)
  | CreateNewSubsection (Array Int)
  | CreateNewSection (Array Int)

data Query a = ReceiveTOCs (TOCTree) a

type State =
  { docID :: DH.DocumentID
  , documentName :: String
  , tocEntries :: RootTree ShortendTOCEntry
  , mSelectedTocEntry :: Maybe Int
  , showAddMenu :: Array Int
  }

tocview
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => H.Component Query Input Output m
tocview = H.mkComponent
  { initialState: \input ->
      { documentName: ""
      , tocEntries: Empty
      , mSelectedTocEntry: Nothing
      , showAddMenu: [ -1 ]
      , docID: input
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where

  render :: State -> forall slots. H.ComponentHTML Action slots m
  render state =
    HH.div_
      ( rootTreeToHTML state.documentName state.showAddMenu state.mSelectedTocEntry
          state.tocEntries
      )

  handleAction :: Action -> forall slots. H.HalogenM State Action slots Output m Unit
  handleAction = case _ of

    Init -> do
      s <- H.get
      mDoc <- H.liftAff $ Request.getDocumentHeader s.docID
      let
        docName = case mDoc of
          Nothing -> ""
          Just doc -> DH.getName doc
      H.modify_ \st -> do
        st { documentName = docName }

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
    :: String
    -> Array Int
    -> Maybe Int
    -> RootTree ShortendTOCEntry
    -> forall slots
     . Array (H.ComponentHTML Action slots m)
  rootTreeToHTML _ _ _ Empty = []
  rootTreeToHTML docName menuPath mSelectedTocEntry (RootTree { children }) =
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
                , renderAddButton menuPath []
                ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "toc-list" ] ]
            ( concat $ mapWithIndex
                ( \ix (Edge child) ->
                    treeToHTML menuPath 1 mSelectedTocEntry [ ix ] child
                )
                children
            )
        ]
    ]

  treeToHTML
    :: Array Int
    -> Int
    -> Maybe Int
    -> Array Int
    -> Tree ShortendTOCEntry
    -> forall slots
     . Array (H.ComponentHTML Action slots m)
  treeToHTML menuPath level mSelectedTocEntry path (Node { title, children }) =
    let
      nodeClasses =
        [ HH.ClassName "toc-item", HB.rounded ]

      innerDivClasses =
        [ HB.dFlex, HB.alignItemsCenter, HB.py2, HB.px2, HB.positionRelative ]

      titleClasses =
        [ HB.textTruncate, HB.flexGrow1, HB.fwBold, HB.fs5 ]
    in
      [ HH.div
          [ HP.classes nodeClasses
          , HP.attr (HH.AttrName "data-level") (show level)
          ]
          [ HH.div
              [ HP.classes innerDivClasses ]
              [ HH.span
                  [ HP.classes
                      [ HH.ClassName "toc-drag-handle", HB.textMuted, HB.me2 ]
                  , HP.style ("margin-left: " <> show level <> "rem;")
                  ]
                  [ HH.text "⋮⋮" ]
              , HH.span
                  [ HP.classes titleClasses ]
                  [ HH.text title ]
              , renderAddButton menuPath path
              ]
          ]
      ] <> concat
        ( mapWithIndex
            ( \ix (Edge child) ->
                treeToHTML menuPath (level + 1) mSelectedTocEntry (path <> [ ix ])
                  child
            )
            children
        )
  treeToHTML _ level mSelectedTocEntry _ (Leaf { title, node }) =
    let
      { id, paraID: _, name: _ } = node

      baseClasses =
        [ HH.ClassName "toc-item", HB.rounded ]

      selectedClasses =
        if Just id == mSelectedTocEntry then
          [ HB.bgPrimary, HH.ClassName "bg-opacity-10", HB.textPrimary ]
        else []

      containerProps =
        [ HP.classes (baseClasses <> selectedClasses)
        , HP.attr (HH.AttrName "data-level") (show level)
        , HP.title ("Jump to section " <> title)
        ]

      innerDivBaseClasses =
        [ HB.dFlex, HB.alignItemsCenter, HB.py2, HB.px2, HB.positionRelative ]

      innerDivProps =
        [ HP.classes innerDivBaseClasses
        , HP.style "cursor: pointer;"
        ] <>
          (if level > 0 then [ HE.onClick \_ -> JumpToSection title id ] else [])
    in
      [ HH.div
          containerProps
          [ HH.div
              innerDivProps
              [ HH.span
                  [ HP.classes
                      [ HH.ClassName "toc-drag-handle", HB.textMuted, HB.me2 ]
                  , HP.style ("margin-left: " <> show level <> "rem;")
                  ]
                  [ HH.text "⋮⋮" ]
              , HH.span
                  [ HP.classes
                      [ HB.textTruncate, HB.fwNormal, HB.fs6 ]
                  ]
                  [ HH.text title ]
              ]
          ]
      ]

  -- Helper to render add button with dropdown
  renderAddButton
    :: forall slots. Array Int -> Array Int -> H.ComponentHTML Action slots m
  renderAddButton menuPath currentPath =
    HH.div
      [ HP.classes [ HB.positionRelative ] ]
      [ HH.button
          [ HP.classes
              [ HB.btn
              , HB.btnSm
              , HB.btnOutlineSecondary
              , HB.border0
              , HH.ClassName "toc-add-wrapper"
              ]
          , HE.onClick \_ -> ToggleAddMenu currentPath
          , HP.style "font-size: 0.75rem;"
          ]
          [ HH.text "+" ]
      -- Dropdown menu (only visible when path matches)
      , if menuPath == currentPath then
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
