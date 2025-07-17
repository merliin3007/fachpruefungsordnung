module FPO.Components.TOC where

import Prelude

import Data.Array (concatMap)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Dto.TreeDto (Edge(..), Tree(..))
import FPO.Types (ShortendTOCEntry, TOCTree, shortenTOC)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB

type Input = Unit

data Output = ChangeSection ShortendTOCEntry

data Action
  = Init
  | JumpToSection ShortendTOCEntry

data Query a = ReceiveTOCs (TOCTree) a

type State =
  { tocEntries :: Tree ShortendTOCEntry
  , mSelectedTocEntry :: Maybe Int
  }

tocview :: forall m. MonadAff m => H.Component Query Input Output m
tocview = H.mkComponent
  { initialState: \_ -> { tocEntries: Empty, mSelectedTocEntry: Nothing }
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
      (treeToHTML 0 state.mSelectedTocEntry state.tocEntries)

  handleAction :: Action -> forall slots. H.HalogenM State Action slots Output m Unit
  handleAction = case _ of

    Init -> do
      pure unit

    JumpToSection entry -> do
      H.modify_ \state ->
        state { mSelectedTocEntry = Just entry.id }
      H.raise (ChangeSection entry)

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    ReceiveTOCs entries a -> do
      let
        shortendEntries = map shortenTOC entries
      H.modify_ \state ->
        state { tocEntries = shortendEntries, mSelectedTocEntry = Nothing }
      pure (Just a)

  treeToHTML
    :: Int
    -> Maybe Int
    -> Tree ShortendTOCEntry
    -> forall slots
     . Array (H.ComponentHTML Action slots m)
  treeToHTML _ _ Empty = []
  treeToHTML n mSelectedTocEntry (Node { node, children }) =
    [ HH.div
        [ HP.title ("Jump to section " <> name)
        , HP.style
            ( "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; padding: 0.25rem 0; padding-left: "
                <> (show (1.5 * toNumber n))
                <> "rem;"
            )
        ]
        [ HH.span
            ( ( if n == 0 then []
                else [ HE.onClick \_ -> JumpToSection { id, name } ]
              )
                <>
                  [ HP.classes
                      ( [ HB.textTruncate ]
                          <>
                            if Just id == mSelectedTocEntry then
                              [ HB.fwBold ]
                            else []
                      )
                  , HP.style
                      ( if n == 0 then " font-size: 2rem;"
                        else "cursor: pointer; display: inline-block; min-width: 6ch;"
                          <>
                            if n == 1 then " font-size: 1.25rem;"
                            else ""
                      )
                  ]
            )
            [ HH.text ((if n == 1 then "§" else "") <> name) ]
        ]
    ] <> concatMap
      ( \(Edge { child }) ->
          treeToHTML (n + 1) mSelectedTocEntry child
      )
      children
    where
    { id, name } = node