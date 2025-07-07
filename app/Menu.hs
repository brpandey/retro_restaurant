module Menu
  ( MenuChoice (..),
  )
where

data MenuChoice = GardenVariety | ChickenCombo | MahiMahi
  deriving (Show, Enum, Bounded, Eq)

-- menuTasks :: MenuChoice -> [Task]
