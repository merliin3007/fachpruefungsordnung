module FPO.Components.ErrorToasts
  ( ErrorAction(..)
  , Input
  , Output
  , component
  ) where

import Prelude

import Data.Array (length, mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import FPO.Data.AppError (AppError)
import FPO.Data.Store as Store
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, updateStore)

type State = Array AppError

type Input = Array AppError

type Output = Void

component
  :: forall query m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => H.Component query Input Output m
component =
  H.mkComponent
    { initialState: \input -> input
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< HandleNewErrors
        }
    }

data ErrorAction
  = HandleNewErrors (Array AppError)
  | RemoveToast Int
  | AutoRemoveToast Int

handleAction
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ErrorAction
  -> H.HalogenM State ErrorAction () Output m Unit
handleAction = case _ of
  HandleNewErrors errors -> do
    H.liftEffect $ log $ "ErrorToasts: Received " <> show (length errors) <> " errors"
    H.put errors
    -- Auto-remove toasts after 5 seconds
    void $ H.fork do
      H.liftAff $ delay (Milliseconds 5000.0)
      when (length errors > 0) do
        handleAction $ AutoRemoveToast (length errors - 1)

  RemoveToast index -> do
    H.liftEffect $ log $ "ErrorToasts: Removing toast at index " <> show index
    updateStore $ Store.RemoveError index

  AutoRemoveToast index -> do
    H.liftEffect $ log $ "ErrorToasts: Auto-removing toast at index " <> show index
    updateStore $ Store.RemoveError index

render :: forall m. Array AppError -> H.ComponentHTML ErrorAction () m
render errors = do
  HH.div
    [ HP.class_ (HH.ClassName "fpo-toast-container")
    ]
    [ HH.div_ (mapWithIndex renderToast errors)
    ]

renderToast :: forall m. Int -> AppError -> H.ComponentHTML ErrorAction () m
renderToast index error =
  HH.div
    [ HP.classes
        [ HH.ClassName "fpo-toast"
        , HH.ClassName "fpo-toast-error"
        , HH.ClassName "animate-slide-in"
        ]
    ]
    [ HH.div
        [ HP.class_ (HH.ClassName "fpo-toast-content")
        , HP.style
            "display: flex; justify-content: space-between; align-items: center;"
        ]
        [ HH.span
            [ HP.class_ (HH.ClassName "fpo-toast-message")
            , HP.style "flex: 1; margin-right: 12px;"
            ]
            [ HH.text (show error) ]
        , HH.button
            [ HP.class_ (HH.ClassName "fpo-toast-close")
            , HP.style
                "background: none; border: none; color: #721c24; cursor: pointer; font-size: 20px; font-weight: bold; width: 20px; height: 20px;"
            , HE.onClick \_ -> RemoveToast index
            ]
            [ HH.text "×" ]
        ]
    ]
