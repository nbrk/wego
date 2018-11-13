module WEGO.Client where

import           WEGO.Types

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad
import qualified Data.ByteString.Lazy         as BL
import           Data.Maybe
import           Network.Simple.TCP
import           Text.Printf


-- | Connects and returns channel for writing (control)
spawnClientControl :: Serializable dt => HostName -> ServiceName -> IO (TChan dt)
spawnClientControl host port = do
  chan <- atomically newTChan

  forkIO $ connect host port $
    \(sock, remote) -> do
--      forkIO $ printf "WEGO control stream to %s:%s\n" host port
      forever $ do
        pat <- atomically $ readTChan chan
        Network.Simple.TCP.sendLazy sock (encode pat)

  return chan


-- | Connects and returns channel for reading (view)
spawnClientView :: Serializable dt => HostName -> ServiceName -> IO (TChan dt)
spawnClientView host port = do
  chan <- atomically newTChan

  forkIO $ connect host port $
    \(sock, remote) -> do
--      forkIO $ printf "WEGO view stream to %s:%s\n" host port
      forever $ do
        mbdat <- Network.Simple.TCP.recv sock 4096
        unless (isNothing mbdat) $ do
          let mbpat = decode $ BL.fromStrict (fromJust mbdat)
          case mbpat of
            Just pat -> atomically $ writeTChan chan pat
            Nothing  ->
  --            printf "Garbage data from %s: %s\n"
  --            (show remote) (show mbdat)
              return ()

  return chan


spawnClient' :: Serializable dt => HostName -> (ServiceName, ServiceName) -> IO (TChan dt, TChan dt)
spawnClient' host (ctrlport, viewport) = do
  ctrlchan <- spawnClientControl host ctrlport
  viewchan <- spawnClientView host viewport
  return (ctrlchan, viewchan)


spawnClient :: Serializable dt => String -> (Int, Int) -> IO (TChan dt, TChan dt)
spawnClient h (cp, vp) = spawnClient' h (show cp, show vp)
