module Wbcore.Logic.PrePe where

import Clash.Prelude
import Wbcore.Logic.FlatQueue (flatQueue)
import Wbcore.Logic.FlatQueueAggregator (aggregate)
import Wbcore.Logic.QueueArbiter (queueArbiter)
import Wbcore.Types.Alias (Busy, notBusy)

tokenStream ::
  HiddenClockResetEnable dom =>
  NFDataX operand_t =>
  KnownNat n_operands =>
  KnownNat queue_depth =>
  KnownNat arbiter_width =>
  SNat queue_depth ->
  (operand_t -> operand_t -> Bool) ->
  (operand_t -> Vec n_operands Bool) ->
  Signal dom (Vec (n_operands + 1) (Vec (arbiter_width + 1) (Maybe operand_t))) ->
  Signal dom Bool ->
  (Signal dom (Maybe (Vec (n_operands + 1) operand_t)), Vec (n_operands + 1) (Vec (arbiter_width + 1) (Signal dom Busy)))
tokenStream queueDepth tokenWaveEq tokenSatMask operands backendWannaPop = (aggregatedQueue, frontendBusy)
  where
    arbitratedOperandsWithBusy = zipWith (queueArbiter . unbundle) (unbundle operands) (map (register notBusy) busySig)
    arbitratedOperands = map fst arbitratedOperandsWithBusy
    frontendBusy = map snd arbitratedOperandsWithBusy
    (queueViews, busySig) = unzip $ imap (\i x -> flatQueue queueDepth x (unbundledPopSignals !! i)) arbitratedOperands
    (popSignals, aggregatedQueue) = unbundle $ aggregate tokenWaveEq tokenSatMask $ bundle (bundle queueViews, backendWannaPop)
    unbundledPopSignals = unbundle popSignals
