module Wbcore.Types.DispatchTag where

import Clash.Prelude
import Wbcore.Types.Alias

data DispatchTag = DispatchTag
  { clusterIndex :: ClusterIndex,
    intraCluster :: IntraClusterDispatchTag
  }
  deriving (Show, Eq, Generic, NFDataX)

data IntraClusterDispatchTag = IntraClusterDispatchTag
  { peIndex :: PeIndex,
    slotIndex :: SlotIndex
  }
  deriving (Show, Eq, Generic, NFDataX)
