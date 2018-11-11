{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module WEGO.Types where

import qualified Data.ByteString.Lazy as BL
import           Data.Monoid


-- | A class for serializable data
class Serializable dt where
  -- | Serialize into the lazy bytestring
  encode :: dt -> BL.ByteString

  -- | Deserialize a lazy bytestring into the type
  decode :: BL.ByteString -> Maybe dt


-- | A class for types that can be diffed/patched
class (Eq d) => Diffable d dt where
  -- | Difference between the two
  diff :: d -> d -> dt

  -- | Patch incorporation
  patch :: dt -> d -> Maybe d


-- | The class of simulation states
class SimulationState d where
  -- | Readiness predicate
  ready :: d -> Bool

  -- | Simulation endomorphism
  simulate :: Endo d

  -- | Empty (the default) instantiation
  empty :: d

