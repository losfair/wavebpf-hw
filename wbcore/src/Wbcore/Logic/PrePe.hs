module Wbcore.Logic.PrePe where

import Clash.Prelude
import Wbcore.Logic.FlatQueue (flatQueue)
import Wbcore.Logic.FlatQueueAggregator (aggregate)
import Wbcore.Types.Alias ( Busy )

tokenStream ::
  HiddenClockResetEnable dom =>
  NFDataX operand_t =>
  KnownNat n_operands =>
  KnownNat queue_depth =>
  SNat queue_depth ->
  (operand_t -> operand_t -> Bool) ->
  (operand_t -> Vec n_operands Bool) ->
  Signal dom (Vec (n_operands + 1) (Maybe operand_t)) ->
  Signal dom (Maybe (Vec (n_operands + 1) operand_t), Vec (n_operands + 1) Busy)
tokenStream queueDepth tokenWaveEq tokenSatMask operands = bundle (aggregatedQueue, bundle busySig)
  where
    (queueViews, busySig) = unzip $ imap (\i x -> flatQueue queueDepth x (unbundledPopSignals !! i)) $ unbundle operands
    (popSignals, aggregatedQueue) = unbundle $ aggregate tokenWaveEq tokenSatMask (bundle queueViews)
    unbundledPopSignals = unbundle popSignals
