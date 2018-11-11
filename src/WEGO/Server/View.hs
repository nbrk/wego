module WEGO.Server.View where

import           Control.Concurrent.STM.TChan
import           Control.Monad
import           MVC
import qualified MVC.Prelude                  as MVCP


-- | Write to channel the data acquired from the model
viewFromChan :: TChan a -> View a
viewFromChan chan = asSink $ \a ->
  -- will be distributed over the network
  void $ atomically $ writeTChan chan a


-- viewFromChan' :: TChan () -> View a
-- viewFromChan' chan = asSink $ \_ ->
--   -- will be distributed over the network
--   void $ atomically $ writeTChan chan ()


-- stdoutEvents :: View [Event]
-- stdoutEvents =
--   asSink $ \es -> do
--     putStrLn "*** Events follow ***"
--     putStrLn (concatMap show es)


-- stdoutGame :: View Game
-- stdoutGame =
--   asSink $ \g -> do
--     putStrLn "*** Game state follows ***"
--     print g
