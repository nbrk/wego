{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
module WEGO.Server.Model where

import           WEGO.Server.Controller
import           WEGO.Server.View
import           WEGO.Types

import           Control.Applicative
import           Control.Concurrent           (forkIO, killThread, myThreadId)
import           Control.Concurrent.STM.TChan
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import qualified Data.ByteString.Lazy         as BL
import           Data.Maybe
import           Data.Monoid
import           MVC
import           Network.Simple.TCP
import           Text.Printf


--data Inputs dt = InputsPatch dt | InputsBigPatchRequest ()
data Inputs dt where
  InputsPatch :: dt -> Inputs dt
  InputsBigPatchRequest :: () -> Inputs dt
data Outputs dt = OutputsBigPatch dt | OutputsSmallPatch dt
makePrisms ''Outputs


externalSocketsDefault :: Serializable a => Managed (View (Outputs a), Controller (Inputs a))
externalSocketsDefault = externalSockets HostAny ("20000", "30000")


-- | TCP sockets view-controller pair
externalSockets ::
     (Serializable a)
  => HostPreference
  -> (ServiceName, ServiceName)
  -> Managed (View (Outputs a), Controller (Inputs a))
externalSockets host (portc, portv) = do
  chanctrl <- liftIO $ atomically newTChan
  chanpat <- liftIO $ atomically newTChan
  chanbigreq <- liftIO $ atomically newTChan
  chanbig <- liftIO $ atomically newTChan

  liftIO $ forkIO $ serve host portc $ \(sock, remoteAddr) -> do
    chan <- atomically $ dupTChan chanctrl
    putStrLn $ "Control connection established from " ++ show remoteAddr
    forever $ do
      mbdat <- Network.Simple.TCP.recv sock 4096
      if isJust mbdat
        then
        do
          let mbpat = decode $ BL.fromStrict $ fromJust mbdat
          case mbpat of
            Just pat -> do
              printf "Patch from %s: %s\n" (show remoteAddr) (show mbdat)
              atomically $ writeTChan chan pat
            Nothing  ->
              printf "Garbage data from %s: %s\n" (show remoteAddr) (show mbdat)
        else
        suicideThread

  liftIO $ forkIO $ serve host portv $ \(sock, remoteAddr) -> do
    putStrLn $ "View connection established from " ++ show remoteAddr

    atomically $ writeTChan chanbigreq ()
    bigpat <- atomically $ readTChan chanbig
    Network.Simple.TCP.send sock (BL.toStrict (encode bigpat))

    chan <- atomically $ dupTChan chanpat
    forever $ do
      pat <- atomically $ readTChan chan
      Network.Simple.TCP.send sock (BL.toStrict (encode pat))

  let cs = fmap (fmap InputsPatch) (ctrlFromChan chanctrl) <>
           fmap (fmap InputsBigPatchRequest) (ctrlFromChan chanbigreq)
  let vs = handles _OutputsSmallPatch (viewFromChan chanpat) <>
           handles _OutputsBigPatch (viewFromChan chanbig)

  liftA2 (,) (return vs) cs



model :: (Diffable d dt, SimulationState d) => Model d (Inputs dt) (Outputs dt)
model = asPipe $ forever $ do
  inp <- await

  case inp of
    InputsBigPatchRequest () -> do
      dat <- get
      let pat = diff initial dat
      yield $ OutputsBigPatch pat
    InputsPatch pat -> do
      dat <- get
      let res = patch pat dat
      case res of
        Just newdat -> do

          --
          -- Put the new simstate and distribute the patch
          --
          put newdat
          yield $ OutputsSmallPatch pat

          --
          -- Check if the simstate was marked ready for the sim round
          --
          when (ready newdat) $ do
            let newdat' = appEndo simulate newdat
            put newdat'
            let pat' = diff newdat newdat'
            yield $ OutputsSmallPatch pat'


        Nothing -> error "Can't patch the data!"


suicideThread = myThreadId >>= killThread
