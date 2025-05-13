-- | This module defines the Navigate type class and its instance for the Halogen monad.
-- | This allows us to navigate between different pages in the application.

module FPO.Data.Navigate where

import Prelude

import FPO.Data.Route (Route)
import Halogen (lift)
import Halogen as H

class Monad m <= Navigate m where
  navigate :: Route -> m Unit

instance navigateHalogenM :: Navigate m => Navigate (H.HalogenM st act slots msg m) where
  navigate = lift <<< navigate