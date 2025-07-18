{-# LANGUAGE NumericUnderscores #-}

module Kitchen
  ( openKitchen,
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Time
import qualified KitchenQueue as KQ
import KitchenTypes
import Menu
import qualified Metrics as M
import System.Random

data OrderState = Raw | Preparing | Cooked | Delivered
  deriving (Show, Enum, Bounded, Eq)

data OrderStatus = OrderStatus
  { totalFoodTasks :: Int,
    finishedFoodTasks :: Int,
    foodPrepared :: Bool,
    beveragePrepared :: Bool,
    delivered :: Bool,
    orderState :: OrderState
  }

type StatusMap = TVar (HM.HashMap Int OrderStatus)

newtype CookFail = CookFail Int

nextState :: OrderStatus -> OrderStatus
nextState os@(OrderStatus _ _ _ _ _ Raw) =
  os {orderState = Preparing}
nextState os@(OrderStatus _ _ True True _ Preparing) =
  os {orderState = Cooked}
nextState os@(OrderStatus _ _ _ _ True Cooked) =
  os {orderState = Delivered}
nextState os = os -- No change if conditions not met

openKitchen :: IO ([Int] -> IO (), M.Metrics)
openKitchen = do
  -- Spin up all the actors: orderTakers, cooks, customers, waitresses
  putStrLn "Kitchen starting with 2 order takers, 3 cooks, and 3 waitresses"

  customerQ <- newTQueueIO
  kitchenQ <- atomically $ KQ.newKQ 10
  pendingDeliveryQ <- newTQueueIO
  deliveryQ <- newTQueueIO

  statusMap <- newTVarIO HM.empty

  metrics <- M.createMetrics
  metricsC <- newTChanIO

  void $ forkIO $ M.metricsHandler metrics metricsC

  replicateM_ 2 $ forkIO $ orderTaker customerQ kitchenQ statusMap metricsC

  spawnCooks [1 .. 3] kitchenQ pendingDeliveryQ statusMap metricsC

  forM_ [1 .. 3] $ \wid -> forkIO $ waitress wid pendingDeliveryQ deliveryQ statusMap metricsC

  let launchPatrons ids = forM_ ids $ \pid -> forkIO $ patron pid customerQ

  return (launchPatrons, metrics)

generateOrder :: Int -> TQueue Order -> IO String
generateOrder oid queue = do
  now <- getCurrentTime

  menuItem <- randomEnum :: IO MenuChoice
  priority <- randomEnum :: IO JobTier

  let order = Order now oid oid menuItem priority
  atomically $ writeTQueue queue order

  return $ "Placed new order " ++ show oid ++ " (menu item " ++ show menuItem ++ ")"

patron :: Int -> TQueue Order -> IO ()
patron pid queue = do
  orderOutput <- generateOrder pid queue
  putStrLn $ "Patron " ++ show pid ++ ", " ++ orderOutput

-- Receives order from customerQ, places onto kitchenQ
orderTaker :: TQueue Order -> KQ.KQueue -> StatusMap -> TChan M.OrderEvent -> IO ()
orderTaker fromQ toQ statusMap metricsC = forever $ do
  order <- atomically $ readTQueue fromQ
  putStrLn $ " Received Order " ++ show (orderId order)
  -- take order and split menu item selection into CookJobs
  let mItem = menuChoice order
      (pid, oid, time, taskList, pri) = (patronId order, orderId order, createdAt order, menuTasks mItem, priority order)
      cookJobs = convertTasks pid oid time taskList mItem pri
  atomically $ forM_ cookJobs (\j -> KQ.enqueue j order toQ)
  atomically $ do
    M.publishMetric metricsC M.OrderPlaced
    -- add to statusMap
    let os = OrderStatus (length taskList) 0 False False False Raw
    modifyTVar' statusMap (HM.insert (orderId order) os)
  return ()

spawnCooks :: (Foldable t) => t Int -> KQ.KQueue -> TQueue Order -> StatusMap -> TChan M.OrderEvent -> IO ()
spawnCooks list fromQ toQ statusMap metricsC = do
  cookFail <- newTChanIO
  -- start cooks
  forM_ list $ \cid -> forkIO $ cook cid fromQ toQ statusMap cookFail metricsC
  -- launch cookSupervisor as a new thread to handle on demand any cook failures
  void $ forkIO $ cookSupervisor fromQ toQ statusMap cookFail metricsC

cookSupervisor :: KQ.KQueue -> TQueue Order -> StatusMap -> TChan CookFail -> TChan M.OrderEvent -> IO ()
cookSupervisor fromQ toQ statusMap cookFail metricsC = forever $ do
  CookFail cid <- atomically $ readTChan cookFail
  threadDelay =<< randomRIO (100_000, 200_000)
  void $ forkIO $ cook cid fromQ toQ statusMap cookFail metricsC
  putStrLn $ " Cook " ++ show cid ++ ", restarted and given the chance to cook again"

cook :: Int -> KQ.KQueue -> TQueue Order -> StatusMap -> TChan CookFail -> TChan M.OrderEvent -> IO ()
cook cid fromQ toQ statusMap cookFail metricsC = cookLoop 1 -- Only allow for 1 mistake!
  where
    cookLoop :: Int -> IO ()
    cookLoop 0 = do
      putStrLn $ " Cook " ++ show cid ++ " made too many mistakes"
      atomically $ writeTChan cookFail (CookFail cid)
    cookLoop n = do
      job <- atomically $ KQ.dequeue fromQ
      putStrLn $ " Cook " ++ show cid ++ ", preparing job for order " ++ show (jobId job)
      threadDelay =<< randomRIO (500_000, 800_000)
      mistake <- randomRIO (1, 99 :: Int)
      if mistake <= 10
        then do
          putStrLn $ "  Cook " ++ show cid ++ ", made a mistake on job" ++ show (jobId job)
          atomically $ do
            -- writeTQueue fromQ order -- put order back on original queue
            maybeOrder <- KQ.getOrder (jobOrderId job) fromQ
            whenJustDoSTM maybeOrder $ \order -> do
              KQ.enqueue job order fromQ -- put order back on original Kqueue
              M.publishMetric metricsC M.OrderCookedIncorrectly
          cookLoop $ n - 1
        else do
          atomically $ KQ.markComplete job fromQ
          cookUpdate n job fromQ toQ statusMap
    cookUpdate :: Int -> CookJob -> KQ.KQueue -> TQueue Order -> StatusMap -> IO ()
    cookUpdate n job kq toQ statusMap = do
      foodReady <- atomically $ do
        updateStatus
          (jobOrderId job)
          statusMap
          ( \os ->
              let prepared = foodPrepared os
                  finished = finishedFoodTasks os + 1
                  total = totalFoodTasks os
                  prepared' = finished == total
                  changed = (prepared /= prepared') -- capture if it has toggled
                  os' =
                    os
                      { finishedFoodTasks = finished,
                        foodPrepared = prepared'
                      }
               in (nextState os', changed)
          )
      -- Since food is cooked, put on pending delivery queue, for waitress to poll (to ensure drink is also done)
      when foodReady $ do
        maybeOrder <- atomically $ KQ.getOrder (jobOrderId job) kq
        case maybeOrder of
          Nothing -> putStrLn " Weird lost the order! Can't finish it"
          Just order -> do
            atomically $ do
              writeTQueue toQ order
              M.publishMetric metricsC (M.OrderCooked cid)

            putStrLn $
              "  Food cooked for order "
                ++ show (orderId order)
                ++ " and ready to eat (thanks Cook "
                ++ show cid
                ++ ") assistant preparing drinks "

            void $ forkIO $ prepareDrink order statusMap

      cookLoop n

waitress :: Int -> TQueue Order -> TQueue Order -> StatusMap -> TChan M.OrderEvent -> IO ()
waitress wid fromQ toQ statusMap metricsC = forever $ do
  -- A) this simulates waitress checking if the entire order is done (e.g. pestering cooks)
  order1 <- atomically $ readTQueue fromQ
  atomically $ do
    smap <- readTVar statusMap
    let maybeStatus = HM.lookup (orderId order1) smap
    whenJustDoSTM maybeStatus $ \status -> do
      -- if check expression false retry!
      check (orderState status == (Cooked :: OrderState))
      writeTQueue toQ order1
  -- B) this simulates waitress actually delivering finished orders to customer
  order2 <- atomically $ readTQueue toQ
  threadDelay =<< randomRIO (1_000_000, 2_000_000)
  atomically $ do
    void $
      updateStatus
        (orderId order2)
        statusMap
        ( \o ->
            let o' = o {delivered = True}
             in (nextState o', True)
        )
    M.publishMetric metricsC M.OrderDelivered
  putStrLn $ " Waitress " ++ show wid ++ ", delivered order " ++ show (orderId order2) ++ " to customer "

prepareDrink :: Order -> StatusMap -> IO ()
prepareDrink order statusMap = do
  -- simulate drink preparation
  threadDelay =<< randomRIO (100_000, 200_000)
  atomically $ do
    void $
      updateStatus
        (orderId order)
        statusMap
        ( \o ->
            let o' = o {beveragePrepared = True}
             in (nextState o', True)
        )
  putStrLn $ " Drink prepared for order " ++ show (orderId order)

updateStatus :: Int -> StatusMap -> (OrderStatus -> (OrderStatus, Bool)) -> STM Bool
updateStatus key statusMap f = do
  updateLockedHM key statusMap f False

-- Helper functions
updateLockedHM :: (Eq k, Hashable k) => k -> TVar (HM.HashMap k v) -> (v -> (v, a)) -> a -> STM a
updateLockedHM key locked lambda defaultValue = do
  hm <- readTVar locked
  case HM.lookup key hm of
    Nothing -> return defaultValue
    Just value -> do
      let (value', result) = lambda value
      writeTVar locked (HM.insert key value' hm)
      return result

-- Run STM Action if Maybe is Just
whenJustDoSTM :: Maybe a -> (a -> STM ()) -> STM ()
whenJustDoSTM m f = maybe (pure ()) f m
