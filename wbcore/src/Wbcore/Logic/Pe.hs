module Wbcore.Logic.Pe where

import Clash.Prelude
import Wbcore.Logic.Alu (AluInsn, InsnMemSizeBits)
import Wbcore.Logic.PostPe (outputBuffer)
import Wbcore.Logic.PrePe (tokenStream)
import Wbcore.Types.Alias (Busy)

pe ::
  HiddenClockResetEnable dom =>
  NFDataX token =>
  NFDataX output =>
  KnownNat n =>
  KnownNat queue_depth =>
  SNat queue_depth ->
  ( Signal dom (Maybe (Unsigned InsnMemSizeBits, AluInsn)) ->
    Signal dom (Maybe (Vec 2 token)) ->
    Signal dom (Bool, Maybe output)
  ) ->
  (token -> token -> Bool) ->
  (token -> Vec 1 Bool) ->
  (output -> token) ->
  (output -> Vec ((n + 1) * 2) Bool) ->
  Signal dom (Vec 2 (Vec (n + 1) (Maybe token))) ->
  Signal dom (Vec ((n + 1) * 2) Busy) ->
  Signal dom (Maybe (Unsigned InsnMemSizeBits, AluInsn)) ->
  (Signal dom (Vec ((n + 1) * 2) (Maybe token)), Signal dom (Vec 2 (Vec (n + 1) Busy)))
pe queueDepth aluFunc aluTokenEq aluTokenSatMask aoToToken aoToMask input backendBusy refill = (out, bundle $ bundle <$> frontendBusy)
  where
    -- input = (\x -> trace ("input " Prelude.++ show i Prelude.++ " " Prelude.++ show x) x) <$> input_
    (aq, frontendBusy) = tokenStream queueDepth aluTokenEq aluTokenSatMask input wannaPop
    -- aq = (\x -> trace ("aq " Prelude.++ show i Prelude.++ " " Prelude.++ show x) x) <$> aq_
    (wannaPop, aluOut) = unbundle $ aluFunc refill aq
    -- wannaPop = (\x -> trace ("wannaPop " Prelude.++ show i Prelude.++ " " Prelude.++ show x) x) <$> wannaPop_
    outData = fmap aoToToken <$> aluOut
    outMask = maybe (repeat False) aoToMask <$> aluOut
    -- TODO: Make ALU aware of aggregatedBackendBusy
    (out, _aggregatedBackendBusy) = unbundle $ outputBuffer (bundle (outMask, bundle (outData, backendBusy)))

-- out = (\x -> trace ("out " Prelude.++ show i Prelude.++ " " Prelude.++ show x) x) <$> out_
