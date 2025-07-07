module KitchenQueue
  (
  )
where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.HashSet as HS
import Data.Heap (MinHeap)
import qualified Data.Heap as H
import qualified Data.Map.Strict as HM
import Data.Time

data JobTier = VIP | Car | WalkIn deriving (Eq, Show, Ord, Enum, Bounded) -- Either VIP, eat inside restaurant or eat in Car

data CookTask = CookTask
  { taskId :: Int,
    taskName :: String,
    taskPriority :: Int,
    taskRequires :: [Int]
  }
  deriving (Show)

data CookJob = CookJob
  { jobId :: Int,
    jobName :: String,
    jobPriority :: Int,
    jobCreated :: UTCTime, -- addition over Task
    jobTier :: JobTier, -- addition over Task
    jobRequires :: [Int]
  }
  deriving (Show, Eq)

instance Ord CookJob where
  compare x1 x2 =
    -- defer to tuple compare
    compare (jobPriority x1, jobCreated x1) (jobPriority x2, jobCreated x2)

-- MLPQ multi-level priority queue to replace old kitchen queue which is TQueue Order
data KQueue = KQueue
  { tieredQueues :: TVar (HM.Map JobTier (MinHeap CookJob)),
    size :: Int,
    finishedJobIds :: TVar (HS.HashSet Int)
  }

newKQ :: Int -> IO KQueue
newKQ sz = do
  hm <- newTVarIO HM.empty
  forM_ [minBound .. maxBound] $ \tier -> do
    let heap = H.empty
    atomically $ modifyTVar' hm $ HM.insert tier heap
  finishedJobs <- newTVarIO HS.empty
  return $ KQueue hm sz finishedJobs
