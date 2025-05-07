module Editor where

import Prelude

import Ace.Types (Editor)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Ace (ace, editNode) as Ace
import Halogen as H
import Halogen.Themes.Bootstrap5 (flexFill, flexGrow0, flexGrow1, h100, mh100) as HB
import Halogen.HTML as HH
import Halogen.HTML.Properties (classes, ref) as HP

type State =
    { key :: Maybe String
    , editor :: Maybe Editor
    }

data Action = Init


editor :: forall query input output m. MonadEffect m => H.Component query input output m
editor = H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval
        { initialize = Just Init
        , handleAction = handleAction }
    }
    where
    initialState :: State
    initialState = { key: Nothing, editor: Nothing }

    render :: State -> forall action. H.ComponentHTML action () m
    render _ = HH.div [ HP.ref (H.RefLabel "container"), HP.classes [ HB.h100 ] ] []

    handleAction :: MonadEffect m => Action -> H.HalogenM State Action () output m Unit
    handleAction = case _ of
        Init -> do
            H.getHTMLElementRef (H.RefLabel "container") >>= traverse_ \el -> do
                editor_ ← H.liftEffect $ Ace.editNode el Ace.ace
                H.put { key: Just "Hello", editor: Just editor_ }
