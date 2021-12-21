module Wbcore.Logic.Cluster where

import Clash.Prelude
import Wbcore.Logic.Alu (AluIndexBits, AluInsn, AluOutput (ao_mask, ao_token), AluToken (at_addr, at_satmask, at_wave), InsnMemSizeBits, PortIndexBits, alu)
import Wbcore.Logic.PostPe (outputBuffer)
import Wbcore.Logic.PrePe (tokenStream)
import Wbcore.Types.Alias (Busy, notBusy)

type QueueDepth = 5

aluTokenEq :: AluToken -> AluToken -> Bool
aluTokenEq a b = at_addr a == at_addr b && at_wave a == at_wave b

cluster ::
  HiddenClockResetEnable dom =>
  Signal dom (Maybe (Unsigned AluIndexBits, Unsigned InsnMemSizeBits, AluInsn)) ->
  Signal dom (Maybe AluOutput) ->
  Signal dom (Maybe AluToken)
cluster refill rawExtOutput = (\x -> (x !! (0 :: Int)) !! (1 :: Int)) <$> (peInputs !! (0 :: Int)) -- XXX: Using `head` here causes Clash synthesis error
  where
    (extOutput, extOutBusy) = extOutputWrapper rawExtOutput
    refillMux =
      ( \case
          Nothing -> replicate (SNat :: SNat (2 ^ AluIndexBits - 1)) Nothing
          Just (i, a, b) -> replace i (Just (a, b)) (replicate (SNat :: SNat (2 ^ AluIndexBits - 1)) Nothing)
      )
        <$> refill
    aluIqs_ = imap (\i x -> fullPe (peInputs !! (i + 1)) (peBusy !! (i + 1)) x) $ unbundle refillMux
    aluIqs = (extOutput, extOutBusy) :> aluIqs_
    peInputs =
      map
        ( fmap
            ( \x ->
                gather x ($(listToVecTH ([0, 2, 4, 6, 8, 10, 12, 14] :: [Int])) :: Vec 8 Int)
                  :> gather x ($(listToVecTH ([1, 3, 5, 7, 9, 11, 13, 15] :: [Int])) :: Vec 8 Int)
                  :> Nil
            )
            . fst
        )
        aluIqs
    peBusy =
      map
        ( fmap
            ( \x ->
                imap (\i _x -> (x !! (i `mod` 2)) !! (i `div` 2)) (replicate (SNat :: SNat (2 ^ PortIndexBits)) ())
            )
            . snd
        )
        aluIqs

extOutputWrapper ::
  HiddenClockResetEnable dom =>
  Signal dom (Maybe AluOutput) ->
  (Signal dom (Vec (2 ^ PortIndexBits) (Maybe AluToken)), Signal dom (Vec 2 (Vec (2 ^ AluIndexBits) Busy)))
extOutputWrapper x = (out, pure (repeat (repeat notBusy)))
  where
    outData = fmap ao_token <$> x
    outMask = maybe (repeat False) ao_mask <$> x
    (out, _aggregatedBackendBusy) = unbundle $ outputBuffer (bundle (outMask, bundle (outData, pure (repeat notBusy) :: Signal dom (Vec 16 Busy))))

fullPe ::
  HiddenClockResetEnable dom =>
  Signal dom (Vec 2 (Vec (2 ^ AluIndexBits) (Maybe AluToken))) ->
  Signal dom (Vec (2 ^ PortIndexBits) Busy) ->
  Signal dom (Maybe (Unsigned InsnMemSizeBits, AluInsn)) ->
  (Signal dom (Vec (2 ^ PortIndexBits) (Maybe AluToken)), Signal dom (Vec 2 (Vec (2 ^ AluIndexBits) Busy)))
fullPe input backendBusy refill = (out, bundle $ bundle <$> frontendBusy)
  where
    (aq, frontendBusy) = tokenStream (SNat :: SNat QueueDepth) aluTokenEq (\x -> testBit (at_satmask x) 0 :> Nil) input wannaPop
    (wannaPop, aluOut) = unbundle $ alu refill aq
    outData = fmap ao_token <$> aluOut
    outMask = maybe (repeat False) ao_mask <$> aluOut
    -- TODO: Make ALU aware of aggregatedBackendBusy
    (out, _aggregatedBackendBusy) = unbundle $ outputBuffer (bundle (outMask, bundle (outData, backendBusy)))
