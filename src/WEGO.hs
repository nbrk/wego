module WEGO
  ( module WEGO.Types
  , runServer
  , spawnClient
  , HostPreference(..)
  )
where

import           WEGO.Client
import           WEGO.Server
import           WEGO.Types

import           Network.Simple.TCP (HostPreference (..))
