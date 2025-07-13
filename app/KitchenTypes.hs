{-# LANGUAGE ScopedTypeVariables #-}

module KitchenTypes
  ( Order (..),
    JobTier (..),
    CookJob (..),
    Task (..),
    MenuChoice (..),
    randomEnum,
  )
where

import Data.Time
import System.Random (randomRIO)

randomEnum :: forall a. (Enum a, Bounded a) => IO a
randomEnum = toEnum <$> randomRIO (fromEnum (minBound :: a), fromEnum (maxBound :: a))

data MenuChoice = GardenVariety | ChickenCombo | MahiMahi
  deriving (Show, Enum, Bounded, Eq)

data Order = Order
  { createdAt :: UTCTime,
    orderId :: Int,
    patronId :: Int,
    menuChoice :: MenuChoice,
    priority :: JobTier
  }

data JobTier = VIP | Car | WalkIn deriving (Eq, Show, Ord, Enum, Bounded) -- Either VIP, eat inside restaurant or eat in Car

data CookJob = CookJob
  { jobId :: Int,
    jobOrderId :: Int,
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
