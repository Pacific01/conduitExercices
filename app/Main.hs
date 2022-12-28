module Main where

import Conduit
import Data.Bits (Bits(xor))
import Control.Applicative

main :: IO ()
main = do
  -- Pure operations: summing numbers.
  --print $ runConduitPure $ yieldMany [1..10] .| sumC
  --iterEx
  sinkEx

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

--------------------------------------------------------------------------------

-- EXERCISE Rewrite sink to not use do-notation. Hint: it'll be easier to go
-- Applicative.
sink :: Monad m => ConduitT Int o m (String, Int)
sink = do
    x <- takeC 5 .| mapC show .| foldC
    y <- sumC
    return (x, y)

mySink :: Monad m => ConduitT Int o m (String, Int)
mySink = liftA2 (,) (takeC 5 .| mapC show .| foldC) sumC

sinkEx :: IO ()
sinkEx = do
  let res = runConduitPure $ yieldMany [1..10] .| mySink
  print res

-- Self evaluation: Just RTFM https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Applicative.html#t:Applicative
