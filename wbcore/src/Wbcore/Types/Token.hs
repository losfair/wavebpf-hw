module Wbcore.Types.Token where

import Clash.Prelude
import Wbcore.Types.Alias
import Wbcore.Types.DispatchTag (DispatchTag)

data Token = Token
  { clusterIndex :: ClusterIndex,
    peIndex :: PeIndex,
    body :: TokenBody
  }
  deriving (Show, Eq, Generic, NFDataX)

data TokenBody = TokenBody
  { slotIndex :: SlotIndex,
    operandIndex :: Unsigned 1,
    operandValue :: DataWord,
    waveCounter :: WaveCounter
  }
  deriving (Show, Eq, Generic, NFDataX)
