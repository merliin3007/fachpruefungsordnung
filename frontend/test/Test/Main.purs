module Test.Main where

import Prelude

import Effect (Effect)
import Test.Login (main) as Login

main :: Effect Unit
main = Login.main
