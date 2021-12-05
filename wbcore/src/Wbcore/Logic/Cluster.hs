module Wbcore.Logic.Cluster where

import Clash.Prelude
import Wbcore.Types.Alias
import Wbcore.Types.Cluster (ClusterOutput)
import Wbcore.Types.InstructionLoad (InstructionLoadBus)
import qualified Wbcore.Types.Memory.Access

cluster ::
  HiddenClockResetEnable dom =>
  Signal dom InstructionLoadBus ->
  Signal dom Wbcore.Types.Memory.Access.Response ->
  ClusterOutput dom
cluster instrLoadBus memRsp = undefined
