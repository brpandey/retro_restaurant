{-# LANGUAGE NumericUnderscores #-}

import Control.Concurrent (threadDelay)
import Kitchen
import qualified Metrics as M

main :: IO ()
main = do
  putStrLn "Starting Restaurant Simulation.."

  (launchPatrons, metrics) <- openKitchen
  launchPatrons [1 .. 5]

  threadDelay 50_000_000

  M.summaryMetrics metrics
  putStrLn " Restaurant simulation ended... "
