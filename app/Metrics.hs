module Metrics
  ( Metrics (..),
    OrderEvent (..),
    createMetrics,
    publishMetric,
    metricsHandler,
    summaryMetrics,
  )
where

import Control.Concurrent.STM
import Control.Monad (forever)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.List (intercalate)
import Data.Time

data Metrics = Metrics
  { placedOrders :: IORef Int,
    cookMistakeOrders :: IORef Int,
    cookSuccessOrders :: IORef (HM.HashMap Int Int),
    deliveredOrders :: IORef Int
  }

data OrderEvent
  = OrderPlaced
  | OrderCookedIncorrectly
  | OrderCooked Int
  | OrderDelivered

createMetrics :: IO Metrics
createMetrics = Metrics <$> newIORef 0 <*> newIORef 0 <*> newIORef HM.empty <*> newIORef 0

publishMetric :: TChan OrderEvent -> OrderEvent -> STM ()
publishMetric = writeTChan

-- use one thread to receive events, hence mutate only IORef variables
metricsHandler :: Metrics -> TChan OrderEvent -> IO ()
metricsHandler m chan = forever $ do
  event <- atomically $ readTChan chan
  case event of
    OrderPlaced -> do
      modifyIORef' (placedOrders m) (+ 1)
    OrderCookedIncorrectly -> do
      modifyIORef' (cookMistakeOrders m) (+ 1)
    OrderCooked cid -> do
      -- Note: 1 is default value if v is Nothing, cid is key
      modifyIORef' (cookSuccessOrders m) (HM.alter (\v -> Just $ maybe 1 (+ 1) v) cid)
    OrderDelivered -> do
      modifyIORef' (deliveredOrders m) (+ 1)

showMap :: (Show k, Show v) => HM.HashMap k v -> String
showMap hm =
  let pairsList = HM.toList hm -- [(k,v)]
      stringsList = map (\(k, v) -> "Cook " ++ show k ++ ": " ++ show v) pairsList
   in intercalate ", " stringsList

summaryMetrics :: Metrics -> IO ()
summaryMetrics m = do
  now <- getZonedTime
  po <- readIORef (placedOrders m)
  mistake <- readIORef (cookMistakeOrders m)
  cookedMap <- readIORef (cookSuccessOrders m)
  delivered <- readIORef (deliveredOrders m)
  putStrLn $
    "["
      ++ show now
      ++ "]"
      ++ " <Metric Summary> "
      ++ "\n"
      ++ " Orders placed: "
      ++ show po
      ++ "\n"
      ++ " Cook Order Failures: "
      ++ show mistake
      ++ "\n"
      ++ " Orders Cooked by Cook: "
      ++ showMap cookedMap
      ++ "\n"
      ++ " Delivered Orders: "
      ++ show delivered
