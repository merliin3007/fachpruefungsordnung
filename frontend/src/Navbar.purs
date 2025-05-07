module Navbar where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (classes, href) as HP
import Halogen.Themes.Bootstrap5 (bgBodyTertiary, containerFluid, navbar, navbarBrand, navbarExpandLg) as HB

navbar :: forall query input output m. H.Component query input output m
navbar = H.mkComponent
    { initialState: \_ -> unit
    , render
    , eval: H.mkEval H.defaultEval
    }
    where
    render :: Unit -> forall action. H.ComponentHTML action () m
    render _ = HH.nav [ HP.classes [ HB.navbar, HB.navbarExpandLg, HB.bgBodyTertiary ] ]
        [ HH.div [ HP.classes [ HB.containerFluid ] ]
            [ HH.a [ HP.classes [ HB.navbarBrand ], HP.href "#" ] [ HH.text "Navbar" ]

            ]
        ]
