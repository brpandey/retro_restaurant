module Menu
  ( convertTasks,
    menuTasks,
  )
where

import Data.Time
import KitchenTypes

menuTasks :: MenuChoice -> [Task]
menuTasks ChickenCombo =
  [ Task 1 "Grill Chicken" "Grill Jamaican Chicken" 1 [],
    Task 2 "Toast Bun" "Toast Bun" 2 [],
    Task 3 "Make Burger" "Assemble Onions, Cheddar, Avocado, Sour Cream on Bun" 3 [1, 2],
    Task 4 "Fry Side Dish" "Fry Tater Tots" 1 []
  ]
menuTasks GardenVariety =
  [ Task 1 "Prepare Mushrooms" "Cut stems, Soak in Olive Oil & Vinegar, Add Salt + Pepper" 1 [],
    Task 2 "Grill Mushrooms" "Grill Portabello Mushrooms, both sides, until tender" 2 [1],
    Task 3 "Toast Bun" "Toast Bun" 2 [],
    Task 4 "Make Burger" "Assemble Burger, add Tomatoes and Pesto Sauce" 2 [1, 2, 3],
    Task 5 "Make Salad" "Prepare Green salad with Corn, Croutons and Sesame Seeds" 3 [],
    Task 6 "Fry Side Dish" "Fry Onion Rings" 2 []
  ]
menuTasks MahiMahi =
  [ Task 1 "Grill Fish" "Grill Mahi Mahi with Lemon Juice" 1 [],
    Task 2 "Toast Bun" "Toast Bun " 2 [],
    Task 3 "Make Sauce" "Mix Greek yogurt, garlic, lemon juice, relish for a healthier tartar sauce" 2 [],
    Task 4 "Make Burger" "Assemble Burger patty, buns with lettuce and custom sauce" 2 [1, 2, 3]
  ]

convertTasks :: Int -> Int -> UTCTime -> [Task] -> MenuChoice -> JobTier -> [CookJob]
convertTasks patronId oid now taskList menuItem tier =
  let convertId i = patronId * 50 + i
      convert (Task tid name desc priority requires) =
        CookJob
          (convertId tid)
          oid
          (show menuItem ++ " " ++ name ++ " for Patron " ++ show patronId)
          desc
          priority
          now
          tier
          (map convertId requires)
   in map convert taskList
