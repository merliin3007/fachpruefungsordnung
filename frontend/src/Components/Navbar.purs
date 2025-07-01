-- | Navbar component for the application.
-- | It contains links to different pages and a brand name.
-- |
-- | This also serves as a guide for how to implement navigation in this application
-- | using the Navigate type class. Refer to the implementations of `render` and `handleAction`
-- | for more details on how to use the `Navigate` class.

module FPO.Components.Navbar where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff, liftAff)
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (getIgnore, getUser)
import FPO.Data.Route (Route(..))
import FPO.Data.Store (User, saveLanguage)
import FPO.Data.Store as Store
import FPO.Page.HTML (addClass)
import FPO.Translations.Translator
  ( FPOTranslator(..)
  , fromFpoTranslator
  , getTranslatorForLanguage
  )
import FPO.Translations.Util (FPOState)
import Halogen (AttrName(..), ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (attr, classes, style) as HP
import Halogen.HTML.Properties.ARIA (role)
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)

type State = FPOState (user :: Maybe User, language :: String)

data Action
  = Navigate Route
  | Receive (Connected Store.Store Unit) -- Receive store updates
  | Logout
  | SetLanguage String
  | ReloadUser

data Query a
  -- | Request a reload from outside - usually, the main routing component
  -- | will call this to ensure the user data is up-to-date (e.g., after login).
  = RequestReloadUser a

-- | The navbar component that renders the navigation bar.
navbar
  :: forall output m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => H.Component Query Unit output m
navbar = connect (selectEq identity) $ H.mkComponent
  { initialState: \{ context: store } ->
      { user: Nothing
      , language: store.language
      , translator: fromFpoTranslator store.translator
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just ReloadUser
      , handleQuery = handleQuery
      }
  }
  where
  render :: State -> H.ComponentHTML Action () m
  render state = HH.nav
    [ HP.classes [ HB.navbar, HB.navbarExpandSm, HB.bgBodyTertiary ] ]
    [ HH.div [ HP.classes [ HB.containerFluid ] ]
        [ HH.a
            [ HP.classes [ HB.navbarBrand ]
            , HE.onClick (const $ Navigate Home)
            , HP.style "cursor: pointer"
            ]
            [ HH.text "FPO-Editor" ]
        , HH.div [ HP.classes [ HB.navbarCollapse ] ]
            [ HH.ul [ HP.classes [ HB.navbarNav, HB.meAuto ] ]
                ( [ HH.li [ HP.classes [ HB.navItem ] ]
                      [ navButton
                          (translate (label :: _ "common_home") state.translator)
                          Home
                      ]
                  , HH.li [ HP.classes [ HB.navItem ] ]
                      [ navButton "Editor" Editor ]
                  ]
                    <>
                      if (maybe false _.isAdmin state.user) then
                        [ HH.li [ HP.classes [ HB.navItem ] ]
                            [ navButton
                                ( translate (label :: _ "navbar_users")
                                    state.translator
                                )
                                AdminViewUsers
                            ]
                        , HH.li [ HP.classes [ HB.navItem ] ]
                            [ navButton
                                ( translate (label :: _ "navbar_groups")
                                    state.translator
                                )
                                AdminViewGroups
                            ]
                        , HH.li [ HP.classes [ HB.navItem ] ]
                            [ navButton
                                ( translate (label :: _ "navbar_documents")
                                    state.translator
                                )
                                (ViewGroupDocuments 1)
                            ]
                        ]
                      else []
                )

            -- Right side of the navbar
            , HH.ul [ HP.classes [ HB.navbarNav, HB.msAuto ] ]
                [ languageDropdown state.language
                , HH.li [ HP.classes [ HB.navItem ] ]
                    [ case state.user of
                        Nothing -> navButton "Login" Login
                        Just user -> userDropdown state user
                    ]
                ]
            ]
        ]
    ]

  handleAction
    :: forall slots
     . Action
    -> H.HalogenM State Action slots output m Unit
  handleAction (Navigate route) = do
    navigate route
  handleAction (Receive { context: store }) = do
    H.modify_ _
      { language = store.language
      , translator = fromFpoTranslator store.translator
      }
  handleAction Logout = do
    -- Reset the cookies and reset the user state
    _ <- H.liftAff $ getIgnore "/logout"
    H.modify_ _ { user = Nothing }
    -- Simply navigate to the Login page indiscriminately
    navigate Login
  handleAction (SetLanguage lang) = do
    H.liftEffect $ saveLanguage lang
    updateStore $ Store.SetLanguage lang
    -- Build and update the translator for the new language
    let translator = FPOTranslator $ getTranslatorForLanguage lang
    updateStore $ Store.SetTranslator translator
  handleAction ReloadUser = do
    u <- liftAff getUser
    H.modify_ _ { user = u }

  handleQuery :: forall a. Query a -> H.HalogenM State Action () output m (Maybe a)
  handleQuery (RequestReloadUser a) = do
    handleAction ReloadUser
    pure $ Just a

  -- Creates a navigation button.
  navButton :: String -> Route -> H.ComponentHTML Action () m
  navButton label route =
    HH.button
      [ HP.classes [ HB.navLink, HB.btn, HB.btnLink ]
      , HE.onClick (const $ Navigate route)
      ]
      [ HH.text label ]

  -- Creates a user dropdown with user icon and logout option.
  userDropdown :: State -> User -> H.ComponentHTML Action () m
  userDropdown state user =
    HH.li
      [ HP.classes [ HB.navItem, HB.dropdown ] ]
      [ HH.a
          [ HP.classes [ HB.navLink, HB.dropdownToggle ]
          , role "button"
          , HP.attr (AttrName "data-bs-toggle") "dropdown"
          , HP.attr (AttrName "aria-expanded") "false"
          ]
          [ HH.i [ HP.classes [ ClassName "bi-person", HB.me1 ] ] []
          , HH.text user.userName
          ]
      , HH.ul
          [ HP.classes [ HB.dropdownMenu, HB.dropdownMenuEnd ]
          , HP.attr (AttrName "aria-labelledby") "navbarDarkDropdownMenuLink"
          ]
          ( [ dropdownEntry
                (translate (label :: _ "prof_profile") state.translator)
                "person"
                (Navigate (Profile { loginSuccessful: Nothing }))
            ]
              <>
                ( if user.isAdmin then
                    [ dropdownEntry
                        (translate (label :: _ "au_userManagement") state.translator)
                        "person-exclamation"
                        (Navigate AdminViewUsers) `addClass` HB.bgWarningSubtle
                    , dropdownEntry
                        (translate (label :: _ "au_groupManagement") state.translator)
                        "people"
                        (Navigate AdminViewGroups) `addClass` HB.bgWarningSubtle
                    ]
                  else []
                )
              <> [ dropdownEntry "Logout" "box-arrow-right" Logout ]
          )
      ]

  -- Creates a dropdown entry with a label, bootstrap icon, and action.
  dropdownEntry :: String -> String -> Action -> H.ComponentHTML Action () m
  dropdownEntry label icon action =
    HH.li_
      [ HH.span
          [ HP.classes [ HB.dropdownItem, HB.dFlex, HB.alignItemsCenter ]
          , HP.style "cursor: default;"
          , HE.onClick (const action)
          ]
          [ HH.i [ HP.classes [ ClassName $ "bi-" <> icon, HB.flexShrink0, HB.me2 ] ]
              []
          , HH.text label
          ]
      ]

  -- Language dropdown for the navbar.
  languageDropdown :: String -> H.ComponentHTML Action () m
  languageDropdown currentLang =
    HH.li
      [ HP.classes [ HB.navItem, HB.dropdown ] ]
      [ HH.a
          [ HP.classes [ HB.navLink, HB.dropdownToggle ]
          , role "button"
          , HP.attr (AttrName "data-bs-toggle") "dropdown"
          , HP.attr (AttrName "aria-expanded") "false"
          ]
          [ HH.i [ HP.classes [ ClassName "bi-translate", HB.me1 ] ] []
          , HH.text (if currentLang == "de-DE" then "DE" else "EN")
          ]
      , HH.ul
          [ HP.classes [ HB.dropdownMenu, HB.dropdownMenuEnd ] ]
          [ languageEntry "Deutsch" "de-DE" "flag-de" (currentLang == "de-DE")
          , languageEntry "English" "en-US" "flag-gb" (currentLang == "en-US")
          ]
      ]

  -- Language selection entry for the dropdown.
  languageEntry
    :: String -> String -> String -> Boolean -> H.ComponentHTML Action () m
  languageEntry label code icon isActive =
    HH.li_
      [ HH.span
          [ HP.classes $
              [ HB.dropdownItem, HB.dFlex, HB.alignItemsCenter ]
                <> if isActive then [ HB.active ] else []
          , HP.style "cursor: default;"
          , HE.onClick (const $ SetLanguage code)
          ]
          [ HH.i [ HP.classes [ ClassName $ "bi-" <> icon, HB.flexShrink0, HB.me2 ] ]
              []
          , HH.text label
          ]
      ]
