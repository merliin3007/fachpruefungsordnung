-- | Simple test page for login/register.
-- |
-- | This page is currently not connected to any backend and does not perform any
-- | authentication. 

module FPO.Page.Login (component) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueInput) as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB

data Action
  = ToggleRegister
  | UpdateName String
  | UpdateEmail String
  | UpdatePassword String
  | EmitError String

type State =
  { isRegistering :: Boolean
  , name :: String
  , email :: String
  , password :: String
  , error :: Maybe String
  }

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        }
    }
  where
  initialState :: input -> State
  initialState _ =
    { isRegistering: false
    , name: ""
    , email: ""
    , password: ""
    , error: Nothing
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my5 ] ]
      [ if state.isRegistering then renderRegisterForm state
        else renderLoginForm state
      , HH.div [ HP.classes [ HB.textCenter ] ]
          [ case state.error of
              Just err -> HH.div [ HP.classes [ HB.alert, HB.alertDanger ] ]
                [ HH.text err ]
              Nothing -> HH.text ""
          ]
      ]

  handleAction :: MonadAff m => Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    ToggleRegister -> do
      H.modify_ \state -> state { isRegistering = not state.isRegistering, error = Nothing }
      pure unit
    UpdateName name -> do
      H.modify_ \state -> state { name = name, error = Nothing }
      pure unit
    UpdateEmail email -> do
      H.modify_ \state -> state { email = email, error = Nothing }
      pure unit
    UpdatePassword password -> do
      H.modify_ \state -> state { password = password, error = Nothing }
      pure unit
    EmitError error -> do
      H.modify_ \state -> state { error = Just error }
      pure unit

renderLoginForm :: forall w. State -> HH.HTML w Action
renderLoginForm state =
  HH.div [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my3 ] ]
    [ HH.div [ HP.classes [ HB.colLg4, HB.colMd6, HB.colSm8 ] ]
        [ HH.form
            []
            [ HH.label [ HP.classes [ HB.formLabel ], HP.for "email" ]
                [ HH.text "Email address:" ]
            , HH.div [ HP.classes [ HB.inputGroup, HB.mb4 ] ]
                [ HH.span [ HP.classes [ HB.inputGroupText ] ]
                    [ HH.i [ HP.class_ (H.ClassName "bi-person-fill") ] [] ]
                , HH.input
                    [ HP.type_ HP.InputEmail
                    , HP.classes [ HB.formControl ]
                    , HP.placeholder "yo@example.com"
                    , HP.value state.email
                    , HE.onValueInput UpdateEmail
                    ]
                ]
            , HH.label [ HP.classes [ HB.formLabel ], HP.for "password" ]
                [ HH.text "Password:" ]
            , HH.div [ HP.classes [ HB.inputGroup, HB.mb4 ] ]
                [ HH.span [ HP.classes [ HB.inputGroupText ] ]
                    [ HH.i [ HP.class_ (H.ClassName "bi-lock-fill") ] [] ]
                , HH.input
                    [ HP.type_ HP.InputPassword
                    , HP.classes [ HB.formControl ]
                    , HP.placeholder "Password"
                    , HP.value state.password
                    , HE.onValueInput UpdatePassword
                    ]
                ]
            , HH.div [ HP.classes [ HB.mb4, HB.textCenter ] ]
                [ HH.button
                    [ HP.classes [ HB.btn, HB.btnPrimary ]
                    , HP.type_ HP.ButtonSubmit
                    , HE.onClick $ const (EmitError "Login not implemented!")
                    ]
                    [ HH.text "Login" ]
                ]
            , HH.div [ HP.classes [ HB.textCenter ] ]
                [ HH.button
                    [ HP.classes [ HB.btn, HB.btnLink ]
                    , HP.type_ HP.ButtonButton
                    , HE.onClick $ const ToggleRegister
                    ]
                    [ HH.text "Need an account? Register here" ]
                ]
            ]
        ]
    ]

renderRegisterForm :: forall w. State -> HH.HTML w Action
renderRegisterForm state =
  HH.div [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my3 ] ]
    [ HH.div [ HP.classes [ HB.colLg4, HB.colMd6, HB.colSm8 ] ]
        [ HH.form
            []
            [ HH.label [ HP.classes [ HB.formLabel ], HP.for "name" ]
                [ HH.text "Full Name:" ]
            , HH.div [ HP.classes [ HB.inputGroup, HB.mb4 ] ]
                [ HH.span [ HP.classes [ HB.inputGroupText ] ]
                    [ HH.i [ HP.class_ (H.ClassName "bi-person-badge") ] [] ]
                , HH.input
                    [ HP.type_ HP.InputText
                    , HP.classes [ HB.formControl ]
                    , HP.placeholder "John Doe"
                    , HP.value state.name
                    , HE.onValueInput UpdateName
                    ]
                ]
            , HH.label [ HP.classes [ HB.formLabel ], HP.for "email" ]
                [ HH.text "Email address:" ]
            , HH.div [ HP.classes [ HB.inputGroup, HB.mb4 ] ]
                [ HH.span [ HP.classes [ HB.inputGroupText ] ]
                    [ HH.i [ HP.class_ (H.ClassName "bi-person-fill") ] [] ]
                , HH.input
                    [ HP.type_ HP.InputEmail
                    , HP.classes [ HB.formControl ]
                    , HP.placeholder "yo@example.com"
                    , HP.value state.email
                    , HE.onValueInput UpdateEmail
                    ]
                ]
            , HH.label [ HP.classes [ HB.formLabel ], HP.for "password" ]
                [ HH.text "Password:" ]
            , HH.div [ HP.classes [ HB.inputGroup, HB.mb4 ] ]
                [ HH.span [ HP.classes [ HB.inputGroupText ] ]
                    [ HH.i [ HP.class_ (H.ClassName "bi-lock-fill") ] [] ]
                , HH.input
                    [ HP.type_ HP.InputPassword
                    , HP.classes [ HB.formControl ]
                    , HP.placeholder "Password"
                    , HP.value state.password
                    , HE.onValueInput UpdatePassword
                    ]
                ]
            , HH.div [ HP.classes [ HB.mb4, HB.textCenter ] ]
                [ HH.button
                    [ HP.classes [ HB.btn, HB.btnPrimary ]
                    , HP.type_ HP.ButtonSubmit
                    , HE.onClick $ const (EmitError "Registering not implemented!")
                    ]
                    [ HH.text "Register" ]
                ]
            , HH.div [ HP.classes [ HB.textCenter ] ]
                [ HH.button
                    [ HP.classes [ HB.btn, HB.btnLink ]
                    , HP.type_ HP.ButtonButton
                    , HE.onClick $ const ToggleRegister
                    ]
                    [ HH.text "Already have an account? Login here" ]
                ]
            ]
        ]
    ]