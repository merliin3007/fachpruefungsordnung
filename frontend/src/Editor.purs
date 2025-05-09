module Editor where

import Prelude

import Ace (ace, editNode) as Ace
import Ace.Document (getAllLines) as Document
import Ace.Editor as Editor
import Ace.Types (Editor)
import Ace.EditSession as Session
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (classes, ref) as HP
import Halogen.Themes.Bootstrap5 (h100) as HB

type State =
    { key :: Maybe String
    , editor :: Maybe Editor
    }

data Action = Init

-- We use a query to get the content of the editor
data Query a = RequestContent (Array String -> a) 

editor :: forall input output m. MonadEffect m => H.Component Query input output m
editor = H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval
        { initialize = Just Init
        , handleAction = handleAction
        , handleQuery = handleQuery }
    }
    where
    initialState :: State
    initialState = { key: Nothing, editor: Nothing }

    render :: State -> H.ComponentHTML Action () m
    render _ = HH.div [ HP.ref (H.RefLabel "container"), HP.classes [ HB.h100 ] ] []

    handleAction :: Action -> H.HalogenM State Action () output m Unit
    handleAction = case _ of
        Init -> do
            H.getHTMLElementRef (H.RefLabel "container") >>= traverse_ \el -> do
                editor_ <- H.liftEffect $ Ace.editNode el Ace.ace
                H.put { key: Just "Hello", editor: Just editor_ }

    handleQuery :: forall action a. Query a -> H.HalogenM State action () output m (Maybe a)
    handleQuery = case _ of
        RequestContent cb -> 
            -- Because Session does not provide a way to get all lines directly, 
            -- we need to take another indirect route to get the lines.
            -- Notice that this extra step is not needed for all js calls. 
            -- For example, `Session.getLine` can be called directly. 
            H.gets _.editor >>= traverse \ed -> do
                lines <- H.liftEffect $ Editor.getSession ed 
                                    >>= Session.getDocument 
                                    >>= Document.getAllLines
                pure $ cb lines