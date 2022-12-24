module Main where

import Conduit
import Data.Bits (Bits(xor))

main :: IO ()
main = do
  -- Pure operations: summing numbers.
  print $ runConduitPure $ yieldMany [1..10] .| sumC
  iterEx

-- Exercise Work through what happens when we add .| mapM_C print to the mix
-- above.
-- Thought: mapM_C print ::           ConduitT String IO ()  IO ()
-- Reality: mapM_C print :: Show a => ConduitT a      o       IO ()

--------------------------------------------------------------------------------

-- EXERCISE Implement iterMC in terms of mapMC.
iterEx :: IO ()
iterEx = do
  --res <- runConduit $ yieldMany [1..10] .| iterMC print .| sumC
  res <- runConduit $ yieldMany [1..10] .| mapMC (myIterMC print) .| sumC
  print res

myIterMC f x = do
  f x
  return x

-- Self Evaluation: Technically I should have implement iterMC with mapMC not as
-- a parameter of mapMC.
-- Like this: iterM f = mapM (\a -> f a >>= \() -> return a)
