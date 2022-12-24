module Main where

import Conduit

main :: IO ()
main = do
  -- Pure operations: summing numbers.
  print $ runConduitPure $ yieldMany [1..10] .| sumC
