module Main (main) where

import Config

main :: IO ()
main = do
  c <- Config.readConfig
  print c

