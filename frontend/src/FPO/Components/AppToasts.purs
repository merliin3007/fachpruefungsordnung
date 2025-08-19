module FPO.Components.AppToasts
  ( Input
  , Output
  , component
  ) where

import Prelude

import Data.Array (any, filter, length)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import FPO.Data.AppToast (AppToastWithId, ToastId, classForToast)
import FPO.Data.Store as Store
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, updateStore)

type State =
  { toasts :: Array AppToastWithId
  , totalToasts :: Int
  }

type Input = Array AppToastWithId

type Output = Void

component
  :: forall query m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => H.Component query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< HandleNewToasts
        }
    }

data ToastAction
  = HandleNewToasts (Array AppToastWithId)
  | RemoveToast ToastId

handleAction
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ToastAction
  -> H.HalogenM State ToastAction () Output m Unit
handleAction = case _ of
  HandleNewToasts newToasts -> do
    state <- H.get
    let previouslyUnknownToasts = getNewToasts state.toasts newToasts
    H.put { toasts: newToasts, totalToasts: length previouslyUnknownToasts }

    traverse_
      ( \toast -> do
          let toastId = toast.id
          -- Trigger the auto-remove action for each new toast
          H.fork $ do
            H.liftAff $ delay (Milliseconds 5500.0)
            handleAction $ RemoveToast toastId
            pure unit
      )
      previouslyUnknownToasts

  RemoveToast id -> do
    state <- H.get
    let updatedToasts = removeToastWithId id state.toasts
    H.put state { toasts = updatedToasts }
    updateStore $ Store.SetToasts updatedToasts

initialState :: Input -> State
initialState toasts = { toasts: toasts, totalToasts: length toasts }

render :: forall m. State -> H.ComponentHTML ToastAction () m
render state = do
  HH.div
    [ HP.class_ (HH.ClassName "fpo-toast-container")
    ]
    [ HH.div_ (map renderToast state.toasts)
    ]

renderToast :: forall m. AppToastWithId -> H.ComponentHTML ToastAction () m
renderToast toast =
  HH.div
    [ HP.classes
        [ HH.ClassName "fpo-toast"
        , classForToast toast.toast
        , HH.ClassName "animate-slide-in"
        ]
    ]
    [ HH.div
        [ HP.class_ (HH.ClassName "fpo-toast-content")
        ]
        [ HH.span
            [ HP.class_ (HH.ClassName "fpo-toast-message")
            ]
            [ HH.text (show toast.toast) ]
        , HH.button
            [ HP.class_ (HH.ClassName "fpo-toast-close")
            , HE.onClick \_ -> RemoveToast toast.id
            ]
            [ HH.text "x" ]
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "fpo-toast-progress")
        ]
        [ HH.div
            [ HP.class_ (HH.ClassName "fpo-toast-progress-bar")
            , HP.attr (HH.AttrName "data-toast-id") (show toast.id)
            ]
            []
        ]
    ]

doesNotContainElement :: Array AppToastWithId -> AppToastWithId -> Boolean
doesNotContainElement toasts element = not $ any
  (\toast -> toast.id == element.id)
  toasts

getNewToasts :: Array AppToastWithId -> Array AppToastWithId -> Array AppToastWithId
getNewToasts oldToasts newToasts' = filter (doesNotContainElement oldToasts)
  newToasts'

removeToastWithId :: ToastId -> Array AppToastWithId -> Array AppToastWithId
removeToastWithId id toasts = filter (\toast -> toast.id /= id) toasts
