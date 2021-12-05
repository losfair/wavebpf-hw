module Wbcore.Types.DispatchAcceptor where

import Clash.Prelude
import Wbcore.Types.Alias
import Wbcore.Types.DispatchTag (DispatchTag (DispatchTag), IntraClusterDispatchTag (IntraClusterDispatchTag))

data DispatchAcceptor = DispatchAcceptor
  { clusterIndex :: ClusterIndex,
    peIndex :: PeIndex
  }
  deriving (Show, Eq, Generic, NFDataX)

matchesDispatchTag :: DispatchAcceptor -> DispatchTag -> Bool
matchesDispatchTag (DispatchAcceptor ci pi_) (DispatchTag ci' (IntraClusterDispatchTag pi' _)) =
  ci == ci' && pi_ == pi'

matchesIntraClusterDispatchTag :: DispatchAcceptor -> IntraClusterDispatchTag -> Bool
matchesIntraClusterDispatchTag (DispatchAcceptor _ pi_) (IntraClusterDispatchTag pi' _) =
  pi_ == pi'
