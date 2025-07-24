module KitchenQueue
  ( KQueue (..),
    newKQ,
    enqueue,
    dequeue,
    markComplete,
    getOrder,
  )
where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.HashSet as HS
import Data.Heap (MinHeap)
import qualified Data.Heap as H
import qualified Data.Map.Strict as HM
-- import Data.Time
import Debug.Trace (trace) -- Debug only
import KitchenTypes

-- MLPQ multi-level priority queue to replace old kitchen queue which is TQueue Order
data KQueue = KQueue
  { queueMap :: TVar (HM.Map JobTier (MinHeap CookJob)), -- map of tiered priority queues
    size :: Int, -- max size of any one particular priority queue
    finishedJobs :: TVar (HS.HashSet Int), -- finished Job Ids
    orderMap :: TVar (HM.Map Int Order)
  }

newKQ :: Int -> STM KQueue
newKQ sz = do
  hm <- newTVar HM.empty
  forM_ [minBound .. maxBound] $ \tier -> do
    let heap = H.empty
    modifyTVar' hm $ HM.insert tier heap
  finished <- newTVar HS.empty
  hm2 <- newTVar HM.empty

  return $ KQueue hm sz finished hm2

enqueue :: CookJob -> Order -> KQueue -> STM ()
enqueue job order (KQueue qMap sz _ oMap) = do
  let qKey = jobTier job
      oKey = orderId order

  modifyTVar' oMap $ \hm ->
    if HM.member oKey hm
      then hm
      else HM.insert oKey order hm

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
dequeue (KQueue qMap _ finished _) = do
  qm <- readTVar qMap
  tierLevel [minBound .. maxBound :: JobTier] qm
  where
    tierLevel [] _ = retry
    tierLevel (h : t) qm = do
      case HM.lookup h qm of
        Nothing -> tierLevel t qm
        Just pq -> do
          filteredPQ <- filterM (\j -> runnableJob j finished) (H.toList pq)
          case filteredPQ of
            [] -> tierLevel t qm
            (job : _) -> do
              let pq' = H.filter (/= job) pq
              writeTVar qMap $ HM.insert h pq' qm
              trace -- Debug output
                ( "   -> Dequeued job from tier priority q: "
                    ++ show h
                    ++ ", Id: "
                    ++ show (jobId job)
                    ++ ", Order "
                    ++ show (jobOrderId job)
                    ++ ", Prio: "
                    ++ show (jobPriority job)
                    ++ ", Name: "
                    ++ show (jobName job)
                    ++ ", Reqs: "
                    ++ show (jobRequires job)
                )
                (return job) -- return job

markComplete :: CookJob -> KQueue -> STM ()
markComplete job (KQueue _ _ finished _) = do
  modifyTVar' finished $ HS.insert (jobId job)

runnableJob :: CookJob -> TVar (HS.HashSet Int) -> STM Bool
runnableJob job ids = do
  finished <- readTVar ids
  let requiresList = jobRequires job
      -- verify all ids in requires list are finished
      finishedLookup x = flip HS.member finished x
  return $ all finishedLookup requiresList

getOrder :: Int -> KQueue -> STM (Maybe Order)
getOrder oid (KQueue _ _ _ oMap) = do
  hm <- readTVar oMap
  return $ HM.lookup oid hm
