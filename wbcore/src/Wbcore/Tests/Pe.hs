{-# LANGUAGE NumericUnderscores #-}

module Wbcore.Tests.Pe where

import Clash.Explicit.Testbench
import Clash.Prelude (SNat, Vec (Nil, (:>)))
import qualified Clash.Prelude as P
import qualified GHC.IO
import Wbcore.Logic.Alu (AluInsn, AluOutput (AluOutput, ao_mask, ao_token), AluToken (AluToken, at_satmask), InsnMemSizeBits, alu, aluTokenEq)
import Wbcore.Logic.Pe (pe)
import Wbcore.Types.Alias (Busy (Busy), notBusy)
import Prelude

xInst ::
  P.HiddenClockResetEnable dom =>
  P.Signal
    dom
    ( Maybe (P.Unsigned InsnMemSizeBits, AluInsn),
      P.Vec 2 (P.Vec 8 (Maybe AluToken)),
      P.Vec 16 Busy
    ) ->
  P.Signal dom (Vec 16 (Maybe AluToken), Vec 2 (Vec 8 Busy))
xInst x = P.bundle out
  where
    (refill, input, backendBusy) = P.unbundle x
    out = pe (P.SNat :: SNat 5) alu aluTokenEq (\y -> P.testBit (at_satmask y) 0 :> Nil) ao_token ao_mask input backendBusy refill

topEntity ::
  P.Clock P.System ->
  P.Reset P.System ->
  P.Enable P.System ->
  P.Signal
    P.System
    ( Maybe (P.Unsigned InsnMemSizeBits, AluInsn),
      P.Vec 2 (P.Vec 8 (Maybe AluToken)),
      P.Vec 16 Busy
    ) ->
  P.Signal P.System (Vec 16 (Maybe AluToken), Vec 2 (Vec 8 Busy))
topEntity = P.exposeClockResetEnable xInst

testBench :: P.Signal P.System Bool
testBench = done
  where
    testInput =
      stimuliGenerator
        clk
        rst
        $( P.listToVecTH
             [ -- Kickoff
               (Just (0, (0b0000 :: P.BitVector 4) P.++# (30 :: P.BitVector 6) P.++# (0b0000_0000_0000_0100 :: P.BitVector 16) P.++# (0 :: P.BitVector 6) P.++# (0 :: P.BitVector 16) P.++# (0 :: P.BitVector 8) P.++# (0 :: P.BitVector 8)), P.repeat $ P.repeat Nothing, P.repeat notBusy) :: (Maybe (P.Unsigned InsnMemSizeBits, AluInsn), P.Vec 2 (P.Vec 8 (Maybe AluToken)), P.Vec 16 Busy), -- 1
               -- LoadConst 20
               (Just (1, (0b0000 :: P.BitVector 4) P.++# (31 :: P.BitVector 6) P.++# (0b0000_0000_0000_0001 :: P.BitVector 16) P.++# (0 :: P.BitVector 6) P.++# (20 :: P.BitVector 16) P.++# (0 :: P.BitVector 8) P.++# (0 :: P.BitVector 8)), P.repeat $ P.repeat Nothing, P.repeat notBusy), -- 2
               -- LoadConst 22
               (Just (2, (0b0000 :: P.BitVector 4) P.++# (30 :: P.BitVector 6) P.++# (0b0000_0000_0000_0001 :: P.BitVector 16) P.++# (0 :: P.BitVector 6) P.++# (22 :: P.BitVector 16) P.++# (0 :: P.BitVector 8) P.++# (0 :: P.BitVector 8)), P.repeat $ P.repeat Nothing, P.repeat notBusy), -- 3
               -- Add
               (Just (3, (0b0001 :: P.BitVector 4) P.++# (30 :: P.BitVector 6) P.++# (0b0000_0000_0000_0010 :: P.BitVector 16) P.++# (0 :: P.BitVector 6) P.++# (0 :: P.BitVector 16) P.++# (0 :: P.BitVector 8) P.++# (1 :: P.BitVector 8)), P.repeat $ P.repeat Nothing, P.repeat notBusy), -- 4
               (Nothing, P.replace 0 (Just $ AluToken 0 0 0 0) (P.repeat Nothing) P.:> P.repeat Nothing P.:> P.Nil, P.repeat notBusy), -- 5
               (Nothing, P.replace 3 (Just $ AluToken 1 0 0 0) (P.repeat Nothing) P.:> P.repeat Nothing P.:> P.Nil, P.repeat notBusy), -- 6
               (Nothing, P.repeat $ P.repeat Nothing, P.repeat notBusy), -- 7
               (Nothing, P.repeat $ P.repeat Nothing, P.repeat notBusy), -- 8
               (Nothing, P.repeat $ P.repeat Nothing, P.repeat notBusy), -- 9
               (Nothing, P.repeat $ P.repeat Nothing, P.repeat notBusy) -- 10
             ]
         )
    expectOutput =
      outputVerifier'
        clk
        rst
        $( P.listToVecTH
             [ (P.repeat Nothing, P.repeat (P.repeat notBusy)) :: (Vec 16 (Maybe AluToken), Vec 2 (Vec 8 Busy)), -- 1
               (P.repeat Nothing, P.repeat (P.repeat notBusy)), -- 2
               (P.repeat Nothing, P.repeat (P.repeat notBusy)), -- 3
               (P.repeat Nothing, P.repeat (P.repeat notBusy)), -- 4
               (P.repeat Nothing, P.repeat (P.repeat notBusy)), -- 5
               (P.repeat Nothing, P.repeat (P.repeat notBusy)), -- 6
               (P.repeat Nothing, P.repeat (P.repeat notBusy)), -- 7
               (P.replace 2 (Just $ AluToken 30 0 0 0) $ P.repeat Nothing, P.repeat (P.repeat notBusy)), -- 8
               (P.replace 0 (Just $ AluToken 31 0 0 20) $ P.repeat Nothing, P.repeat (P.repeat notBusy)), -- 9
               (P.repeat Nothing, P.repeat (P.repeat notBusy)) -- 10
             ]
         )
    done = expectOutput (topEntity clk rst en testInput)
    en = P.enableGen
    clk = tbSystemClockGen (not <$> done)
    rst = P.systemResetGen

run :: IO ()
run = do
  steps <- GHC.IO.evaluate $ length $ takeWhile not $ P.sample testBench
  putStrLn $ "Steps: " ++ show steps
