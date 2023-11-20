module Main where

import Starman
import GHC.IO.Handle
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  starman
