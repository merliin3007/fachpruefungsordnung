-- | This module defines the main entry point of the application and manages
-- | the high-level structure of the app.
-- |
-- | It implements `Main.component`, the root Halogen component responsible for
-- | rendering the appropriate page based on the current route. Additionally,
-- | it includes global components that persist across multiple pages, such as the navbar.

module Main where

import Data.Maybe

import Data.Either (hush)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import FPO.AppM (runAppM)
import FPO.Components.Navbar as Navbar
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Route (Route(..), routeCodec)
import FPO.Data.Store as Store
import FPO.Page.Home as Home
import FPO.Page.Login as Login
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Store.Monad (class MonadStore)
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, Void, bind, const, discard, pure, unit, void, when, ($), (/=), (<$>), (<<<))
import Routing.Duplex as RD
import Routing.Hash (getHash, matchesWith)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- Router and Main Page

type State = { route :: Maybe Route }

data Query a = NavigateQ Route a

data Action = Initialize

_navbar = Proxy :: Proxy "navbar"
_home = Proxy :: Proxy "home"
_login = Proxy :: Proxy "login"

type Slots =
  ( home :: forall q. H.Slot q Void Unit
  , login :: forall q. H.Slot q Void Unit
  , navbar :: forall q. H.Slot q Void Unit
  )

component
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => H.Component Query Unit Void m
component =
  H.mkComponent
    { initialState: const { route: Just Login }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , handleQuery = handleQuery
        }
    }
  where
  render :: State -> H.ComponentHTML Action Slots m
  render state = HH.div_
    [ HH.slot_ _navbar unit Navbar.navbar unit
    , case state.route of
        Nothing -> HH.text "404 Not Found"
        Just p -> case p of
          Home -> HH.slot_ _home unit Home.component unit
          Login -> HH.slot_ _login unit Login.component unit
    ]

  handleAction :: Action -> H.HalogenM State Action Slots Void m Unit
  handleAction = case _ of
    Initialize -> do
      initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getHash
      navigate $ fromMaybe Home initialRoute

  handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Void m (Maybe a)
  handleQuery = case _ of
    NavigateQ dest a -> do
      st <- H.get
      when (st.route /= Just dest) do
        H.modify_ _ { route = Just dest }
      pure $ Just a

--------------------------------------------------------------------------------
-- Main

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let initialStore = { currentUser: Nothing } :: Store.Store
  rootComponent <- runAppM initialStore component
  halogenIO <- runUI rootComponent unit body

  -- Here, we detect changes in the URL hash ...
  void $ liftEffect $ matchesWith (RD.parse routeCodec) \old new ->
    when (old /= Just new) $ launchAff_ do
      -- ... and update the app state accordingly.
      _response <- halogenIO.query $ H.mkTell $ NavigateQ new
      pure unit