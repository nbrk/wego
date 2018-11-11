module WEGO.Server.Controller where

import           Control.Concurrent.STM.TChan
import           Control.Monad
import           MVC
import qualified MVC.Prelude                  as MVCP



-- | Read a chan and control the model with the data
ctrlFromChan :: TChan a -> Managed (Controller a)
ctrlFromChan chan = MVCP.producer unbounded $ forever $ do
  cmd <- lift $ atomically $ readTChan chan
  yield cmd

