module KitchenQueue
  ( newKQ,
    enqueue,
    dequeue,
  )
where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.HashSet as HS
import Data.Heap (MinHeap)
import qualified Data.Heap as H
import qualified Data.Map.Strict as HM
import Data.Time
import KitchenTypes

-- MLPQ multi-level priority queue to replace old kitchen queue which is TQueue Order
data KQueue = KQueue
  { queueMap :: TVar (HM.Map JobTier (MinHeap CookJob)), -- map of tiered priority queues
    size :: Int, -- max size of any one particular priority queue
    finishedJobs :: TVar (HS.HashSet Int) -- finished Job Ids
  }

newKQ :: Int -> STM KQueue
newKQ sz = do
  hm <- newTVar HM.empty
  forM_ [minBound .. maxBound] $ \tier -> do
    let heap = H.empty
    modifyTVar' hm $ HM.insert tier heap
  finished <- newTVar HS.empty
  return $ KQueue hm sz finished

enqueue :: CookJob -> KQueue -> STM ()
enqueue job (KQueue qMap sz _) = do
  let qKey = jobTier job
  currentMap <- readTVar qMap
  maybe
    (pure ())
    ( \pq ->
        if H.size pq >= sz -- apply backpressure and wait for space in pq
          then retry
          else do
            let updatedHeap = H.insert job pq -- update priority queue min heap
            writeTVar qMap $ HM.alter (const (Just updatedHeap)) qKey currentMap
    )
    (HM.lookup qKey currentMap)

dequeue :: KQueue -> STM CookJob
dequeue (KQueue qMap _ finished) = do
  qm <- readTVar qMap
  tierLevel [minBound .. maxBound :: JobTier] qm
  where
    tierLevel [] _ = retry
    tierLevel (head : tail) qm = do
      case HM.lookup head qm of
        Nothing -> tierLevel tail qm
        Just pq -> do
          filteredPQ <- filterM (\j -> runnableJob j finished) (H.toList pq)
          case filteredPQ of
            [] -> tierLevel tail qm
            (job : _) -> do
              let pq' = H.filter (/= job) pq
              writeTVar qMap $ HM.insert head pq' qm
              return job

markComplete :: CookJob -> KQueue -> STM ()
markComplete job (KQueue _ _ finished) = do
  modifyTVar' finished $ HS.insert (jobId job)

runnableJob :: CookJob -> TVar (HS.HashSet Int) -> STM Bool
runnableJob job ids = do
  finished <- readTVar ids
  let requiresList = jobRequires job
      -- verify all ids in requires list are finished
      finishedLookup x = flip HS.member finished x
  return $ all finishedLookup requiresList
