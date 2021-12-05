module Wbcore.Types.Cluster where

import Clash.Prelude
import qualified Wbcore.Types.Memory.Access

data ClusterOutput dom = ClusterOutput
  { memReq :: Signal dom Wbcore.Types.Memory.Access.Request
  }
