-- | This module contains some additional styling helpers and goodies for the FPO app,
-- | useful for creating and reusing CSS styles across components and pages.

module FPO.UI.Style where

import Halogen.HTML as HH

-- | Popover attribute for tooltips. Can be used just like any other HTML attribute, and
-- | adds a tooltip to the element when hovered.
-- |
-- | Example usage:
-- |    `HH.div [ popover "Tooltip text" ] [ HH.text "Hover me!" ]`
popover :: forall r i. String -> HH.IProp r i
popover = HH.attr (HH.AttrName "data-tooltip")
