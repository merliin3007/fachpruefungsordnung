module Control.Alternative.Utils
    ( whenAlt
    )
where

import Control.Applicative (Alternative, empty)

-- | Run an alternative conditionally.
--   Compare 'Control.Monad.when', which is different, but similar.
--   @'whenAlt' b f@ is equivalent to @'Control.Monad.guard' b *> f@
whenAlt :: (Alternative f) => Bool -> f b -> f b
whenAlt True = id
whenAlt False = const empty
