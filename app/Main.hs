{-# LANGUAGE NumericUnderscores #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map.Strict as HM
import Data.Time
import System.Random

data BurgerType = Mushroom | Steak | BlackBean | Fish
  deriving (Show, Enum, Bounded, Eq)

randomBurger :: IO BurgerType
randomBurger = toEnum <$> randomRIO (0, 3)

data Order = Order
  { createdAt :: UTCTime,
    orderId :: Int,
    burgerType :: BurgerType,
    standardCondiments :: Bool,
    guacamole :: Bool
  }

data OrderState = Raw | Preparing | Cooked | Delivered
  deriving (Show, Enum, Bounded, Eq)

data OrderStatus = OrderStatus
  { foodPrepared :: Bool,
    beveragePrepared :: Bool,
    delivered :: Bool,
    orderState :: OrderState
  }

type OSMap = TVar (HM.Map Int OrderStatus)

nextState :: OrderStatus -> OrderStatus
nextState os@(OrderStatus _ _ _ Raw) =
  os {orderState = Preparing}
nextState os@(OrderStatus True True _ Preparing) =
  os {orderState = Cooked}
nextState os@(OrderStatus _ _ True Cooked) =
  os {orderState = Delivered}
nextState os = os -- No change if conditions not met

generateOrder :: Int -> TQueue Order -> IO String 
generateOrder key queue = do
  createdAt <- getCurrentTime
  condiments <- randomRIO (True, False)
  guac <- randomRIO (True, False)
  bType <- randomBurger

  let order = Order createdAt key bType condiments guac
  atomically $ writeTQueue queue order

  return $ "Placed new order " ++ show key ++ " (burgerType " ++ show bType ++ ")"

patron :: Int -> TQueue Order -> IO ()
patron key queue = do
  orderOutput <- generateOrder key queue
  putStrLn $ "Patron " ++ show key ++ ", " ++ orderOutput

-- Receives order from customerQ, places onto kitchenQ
orderTaker :: TQueue Order -> TQueue Order -> OSMap -> IO ()
orderTaker fromQ toQ statusMap = forever $ do
  order <- atomically $ readTQueue fromQ
  putStrLn $ " Received Order " ++ show (orderId order)

  -- add to statusMap
  let os = OrderStatus False False False Raw
  atomically $ modifyTVar' statusMap (HM.insert (orderId order) os)
  atomically $ writeTQueue toQ order
  return ()

cook :: Int -> TQueue Order -> TQueue Order -> OSMap -> IO ()
cook key fromQ toQ statusMap = cookLoop
  where
    cookLoop :: IO ()
    cookLoop = do
      order <- atomically $ readTQueue fromQ
      putStrLn $ " Cook " ++ show key ++ ", preparing burger for order " ++ show (orderId order)
      threadDelay =<< randomRIO (1_000_000, 2_000_000)
      atomically $ do
        updateStatus
          (orderId order)
          statusMap
          ( \o ->
              let o' = o {foodPrepared = True}
               in nextState o'
          )
      -- Since food is cooked, put on pending delivery queue, for waitress to poll (to ensure drink is also done)
      atomically $ writeTQueue toQ order

      putStrLn $ "  Burger cooked for order " ++ show (orderId order) ++ " and ready to eat, assistant preparing drinks "
      _ <- forkIO $ prepareDrink order statusMap

      cookLoop 

waitress :: Int -> TQueue Order -> TQueue Order -> OSMap -> IO ()
waitress wid fromQ toQ statusMap = forever $ do
  -- A) this simulates waitress checking if the entire order is done (e.g. pinging cooks)
  order1 <- atomically $ readTQueue fromQ
  atomically $ do
    smap <- readTVar statusMap
    case HM.lookup (orderId order1) smap of
      Just status -> do
        check (orderState status == (Cooked :: OrderState))
        writeTQueue toQ order1
      Nothing -> return ()
  -- B) this simulates waitress actually delivering finished orders to customer
  order2 <- atomically $ readTQueue toQ
  threadDelay =<< randomRIO (1_000_000, 2_000_000)
  atomically $ do
    updateStatus
      (orderId order2)
      statusMap
      ( \o ->
          let o' = o {delivered = True}
           in nextState o'
      )
  putStrLn $ " Waitress " ++ show wid ++ ", delivered order " ++ show (orderId order2) ++ " to customer "

prepareDrink :: Order -> OSMap -> IO ()
prepareDrink order statusMap = do
  -- simulate drink preparation
  threadDelay =<< randomRIO (100_000, 200_000)
  atomically $ do
    updateStatus
      (orderId order)
      statusMap
      ( \o ->
          let o' = o {beveragePrepared = True}
           in nextState o'
      )
  putStrLn $ " Drink prepared for order " ++ show (orderId order)

updateStatus :: Int -> OSMap -> (OrderStatus -> OrderStatus) -> STM ()
updateStatus key statusMap f = do
  modifyTVar' statusMap (HM.adjust f key)

main :: IO ()
main = do
  putStrLn "Starting Restaurant Simulation.."
  customerQ <- newTQueueIO
  kitchenQ <- newTQueueIO
  pendingDeliveryQ <- newTQueueIO
  deliveryQ <- newTQueueIO

  statusMap <- newTVarIO HM.empty

  -- Spin up all the actors: orderTakers, cooks, customers, waitresses
  putStrLn "Restaurant simulation starting with 2 order takers, 3 cooks, 5 patrons, and  3 waitresses"

  replicateM_ 2 $ forkIO $ orderTaker customerQ kitchenQ statusMap
  forM_ [1 .. 3] $ \cid -> forkIO $ cook cid kitchenQ pendingDeliveryQ statusMap
  forM_ [1 .. 5] $ \pid -> forkIO $ patron pid customerQ
  forM_ [1 .. 3] $ \wid -> forkIO $ waitress wid pendingDeliveryQ deliveryQ statusMap
  threadDelay 50_000_000

  putStrLn " Restaurant simulation ended... "
