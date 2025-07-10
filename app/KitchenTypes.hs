module KitchenTypes
  ( JobTier (..),
    CookJob (..),
    Task (..),
  )
where

import Data.Time

data JobTier = VIP | Car | WalkIn deriving (Eq, Show, Ord, Enum, Bounded) -- Either VIP, eat inside restaurant or eat in Car

data CookJob = CookJob
  { jobId :: Int,
    jobName :: String,
    jobDesc :: String,
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

data Task = Task
  { taskId :: Int,
    taskName :: String,
    taskDesc :: String,
    taskPriority :: Int,
    taskRequires :: [Int]
  }
  deriving (Show)
