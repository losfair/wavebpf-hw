{-# LANGUAGE LambdaCase #-}

module Wbcore.Logic.Cluster where

import Clash.Prelude
import Data.Maybe (isJust)
import Debug.Trace (trace)
import Wbcore.Logic.Alu (AluIndexBits, AluInsn, AluOutput (ao_mask, ao_token), AluToken (at_addr, at_satmask, at_wave), InsnMemSizeBits, PortIndexBits, alu)
import Wbcore.Logic.Pe (pe)
import Wbcore.Logic.PostPe (outputBuffer)
import Wbcore.Types.Alias (Busy, notBusy)
import qualified Prelude

type QueueDepth = 5

aluTokenEq :: AluToken -> AluToken -> Bool
aluTokenEq a b = at_addr a == at_addr b && at_wave a == at_wave b

cluster ::
  HiddenClockResetEnable dom =>
  Signal dom (Maybe (Unsigned AluIndexBits, Unsigned InsnMemSizeBits, AluInsn)) ->
  Signal dom (Maybe AluOutput) ->
  Signal dom (Maybe AluToken)
cluster refill rawExtOutput = (\x -> fold (\a b -> if isJust a then a else b) (x !! (0 :: Int))) <$> (peInputs !! (0 :: Int)) -- XXX: Using `head` here causes Clash synthesis error
  where
    (extOutput, extOutBusy) = extOutputWrapper rawExtOutput
    refillMux =
      ( \case
          Nothing -> replicate (SNat :: SNat (2 ^ AluIndexBits - 1)) Nothing
          Just (i, a, b) -> replace (i - 1) (Just (a, b)) (replicate (SNat :: SNat (2 ^ AluIndexBits - 1)) Nothing)
      )
        <$> refill
    aluIqs_ = imap (\i x -> fullPe (peInputs !! ((fromIntegral i :: Unsigned AluIndexBits) + 1)) (peBusy !! ((fromIntegral i :: Unsigned AluIndexBits) + 1)) x) $ unbundle refillMux
    aluIqs' = (extOutput, extOutBusy) :> aluIqs_
    aluIqs = imap (\i y -> unbundle $ fmap (\x -> trace ("aluIqs [" Prelude.++ show i Prelude.++ "]: " Prelude.++ showX (fst x)) x) $ bundle y) aluIqs'
    peInputs_ =
      unbundle $
        ( \iqs ->
            imap
              ( \i _unused ->
                  map (!! ((fromIntegral i :: Unsigned PortIndexBits) * 2)) iqs :> map (!! (((fromIntegral i :: Unsigned PortIndexBits) * 2) + 1)) iqs :> Nil
              )
              iqs
        )
          <$> bundle (map fst aluIqs)
    -- XXX: `show`-ing a `Signal` causes Clash to stuck
    peInputs = imap (\i y -> fmap (\x -> trace ("peInputs [" Prelude.++ show i Prelude.++ "]: " Prelude.++ showX x) x) y) peInputs_
    peBusy =
      unbundle $
        ( \iqs ->
            imap
              ( \i _unused ->
                  merge (map (\y -> (y !! 0) !! i) iqs) (map (\y -> (y !! 1) !! i) iqs)
              )
              iqs
        )
          <$> bundle (map snd aluIqs)
    --peBusy = imap (\i y -> fmap (\x -> trace ("peBusy [" Prelude.++ show i Prelude.++ "]: " Prelude.++ showX x) x) y) peBusy_
    fullPe = pe (SNat :: SNat QueueDepth) alu aluTokenEq (\y -> testBit (at_satmask y) 0 :> Nil) ao_token ao_mask

extOutputWrapper ::
  HiddenClockResetEnable dom =>
  Signal dom (Maybe AluOutput) ->
  (Signal dom (Vec (2 ^ PortIndexBits) (Maybe AluToken)), Signal dom (Vec 2 (Vec (2 ^ AluIndexBits) Busy)))
extOutputWrapper x = (out, pure (repeat (repeat notBusy)))
  where
    outData = fmap ao_token <$> x
    outMask = maybe (repeat False) ao_mask <$> x
    (out, _aggregatedBackendBusy) = unbundle $ outputBuffer (bundle (outMask, bundle (outData, pure (repeat notBusy) :: Signal dom (Vec 16 Busy))))
