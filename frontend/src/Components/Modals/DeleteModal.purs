module FPO.Components.Modals.DeleteModal
  ( deleteConfirmationModal
  ) where

import Prelude

-- | Copied over. Redundant imports to be removed later
import Data.Array (filter, length, replicate, slice, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Pagination as P
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (getUser)
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Page.HTML (addButton, addCard, addColumn, emptyEntryGen)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Type.Proxy (Proxy(..))


  -- TODO: This only statically shows the modal whenever the user requests
  --       to delete a group. Because of this binary show/hide logic,
  --       we can't use fancy features like modal animations (fade-in, etc.).
  --       Instead, we could use JSS to toggle the modal visibility, but this
  --       would of course require external JavaScript code.
  --       See https://getbootstrap.com/docs/5.3/components/modal/.

  -- requires:
  -- 1. something to determine what object to delete, like an ID
  -- 2. something to derive a label for the object to delete
  -- 3. an action to cancel the deletion
  -- 4. an action to proceed the deletion
  -- 5. a name for the type of the given object
deleteConfirmationModal :: forall w a action. a -> (a -> String) -> action -> (a -> action) -> String -> HH.HTML w action
deleteConfirmationModal objectIdentifier toObjectName cancelAction confirmAction objectTypeName =
  HH.div_
    [ HH.div
        [ HP.classes
            [ HB.modal, HB.fade, HB.show ]
        , HP.id "deleteModal"
        , HP.attr (HH.AttrName "data-bs-backdrop") "static"
        , HP.attr (HH.AttrName "data-bs-keyboard") "false"
        , HP.attr (HH.AttrName "tabindex") "-1"
        , HP.attr (HH.AttrName "aria-labelledby") "deleteModalLabel"
        , HP.attr (HH.AttrName "aria-hidden") "false"
        , HP.style "display: block;"
        ]
        [ HH.div
            [ HP.classes [ HH.ClassName "modal-dialog" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "modal-content" ] ]
                [ HH.div
                    [ HP.classes [ HH.ClassName "modal-header" ] ]
                    [ HH.h1
                        [ HP.classes
                            [ HH.ClassName "modal-title", HH.ClassName "fs-5" ]
                        , HP.id "deleteModalLabel"
                        ]
                        [ HH.text "Confirm Delete" ]
                    , HH.button
                        [ HP.type_ HP.ButtonButton
                        , HP.classes [ HB.btnClose ]
                        , HP.attr (HH.AttrName "data-bs-dismiss") "modal"
                        , HP.attr (HH.AttrName "aria-label") "Close"
                        , HE.onClick (const cancelAction)
                        ]
                        []
                    ]
                , HH.div
                    [ HP.classes [ HB.modalBody ] ]
                    [ HH.text
                        ( "Are you sure you want to delete " <> objectTypeName <> " " <> toObjectName objectIdentifier <>
                            "?"
                        )
                    ]
                , HH.div
                    [ HP.classes [ HB.modalFooter ] ]
                    [ HH.button
                        [ HP.type_ HP.ButtonButton
                        , HP.classes
                            [ HB.btn, HB.btnSecondary ]
                        , HP.attr (HH.AttrName "data-bs-dismiss") "modal"
                        , HE.onClick (const cancelAction)
                        ]
                        [ HH.text "Cancel" ]
                    , HH.button
                        [ HP.type_ HP.ButtonButton
                        , HP.classes [ HB.btn, HB.btnDanger ]
                        , HE.onClick (const $ confirmAction objectIdentifier)
                        ]
                        [ HH.text "Delete" ]
                    ]
                ]
            ]
        ]
    , HH.div
        [ HP.classes
            [ HH.ClassName "modal-backdrop"
            , HH.ClassName "show"
            ]
        ]
        []
    ]
