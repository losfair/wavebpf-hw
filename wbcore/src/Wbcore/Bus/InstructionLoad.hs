module WbCore.Bus.InstructionLoad where

import Clash.Prelude

data Bus = Bus
  { clusterIndex :: Unsigned 32
  , peIndex      :: Unsigned
  , iLoadData :: Unsigned 32
  } deriving (Generic, NFDataX)
