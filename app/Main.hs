module Main where

import Conduit

main :: IO ()
main = do
  -- Pure operations: summing numbers.
  print $ runConduitPure $ yieldMany [1..10] .| sumC

-- Exercise Work through what happens when we add .| mapM_C print to the mix
-- above.
-- Thought: mapM_C print ::           ConduitT String IO ()  IO ()
-- Reality: mapM_C print :: Show a => ConduitT a      o       IO ()
