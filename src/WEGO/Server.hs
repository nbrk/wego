{-# LANGUAGE AllowAmbiguousTypes #-}
module WEGO.Server where

import           WEGO.Server.Model
import           WEGO.Types

import           MVC
import           Network.Simple.TCP


-- -- | Run the server with default host/ports
runServerDefault s0 =
  runMVC s0 model externalSocketsDefault


-- | Run the server on specified host and control/view ports
runServer host (portc, portv) s0 =
  runMVC s0 model (externalSockets host (portc, portv))


