module Wbcore.Gen.ClusterGen where

import Clash.Prelude
import Wbcore.Logic.Alu
import Wbcore.Logic.Cluster (cluster)

clusterMod ::
  Clock XilinxSystem ->
  Reset XilinxSystem ->
  Enable XilinxSystem ->
  Signal XilinxSystem (Maybe (Unsigned AluIndexBits, Unsigned InsnMemSizeBits, AluInsn)) ->
  Signal XilinxSystem (Maybe AluOutput) ->
  Signal XilinxSystem (Maybe AluToken)
clusterMod = exposeClockResetEnable cluster
{-# ANN
  clusterMod
  ( Synthesize
      { t_name = "WbCluster",
        t_inputs =
          [ PortName "clk",
            PortName "rst",
            PortName "en",
            PortName "refill",
            PortName "ext_output"
          ],
        t_output =
          PortName "ext_input"
      }
  )
  #-}
