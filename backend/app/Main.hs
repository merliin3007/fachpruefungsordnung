module Main (main) where

import Lib
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  someFunc
