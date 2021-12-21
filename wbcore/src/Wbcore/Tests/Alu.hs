{-# LANGUAGE NumericUnderscores #-}

module Wbcore.Tests.Alu where

import Clash.Explicit.Testbench
import Clash.Prelude (Vec (Nil, (:>)))
import qualified Clash.Prelude as P
import qualified GHC.IO
import Wbcore.Logic.Alu (AluInsn, AluOutput (AluOutput), AluToken (AluToken), InsnMemSizeBits, alu)
import Prelude

xInst ::
  P.HiddenClockResetEnable dom =>
  P.Signal dom (Maybe (P.Unsigned InsnMemSizeBits, AluInsn), Maybe (P.Vec 2 AluToken)) ->
  P.Signal dom (Bool, Maybe AluOutput)
xInst x = alu refill input
  where
    (refill, input) = P.unbundle x

topEntity ::
  P.Clock P.System ->
  P.Reset P.System ->
  P.Enable P.System ->
  P.Signal P.System (Maybe (P.Unsigned InsnMemSizeBits, AluInsn), Maybe (P.Vec 2 AluToken)) ->
  P.Signal P.System (Bool, Maybe AluOutput)
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
               (Just (0, (0b0000 :: P.BitVector 4) P.++# (30 :: P.BitVector 6) P.++# (0b0000_0000_0000_0001 :: P.BitVector 16) P.++# (0 :: P.BitVector 6) P.++# (0 :: P.BitVector 16) P.++# (0 :: P.BitVector 8) P.++# (0 :: P.BitVector 8)), Nothing) :: (Maybe (P.Unsigned InsnMemSizeBits, AluInsn), Maybe (P.Vec 2 AluToken)), -- 1
               -- LoadConst 20
               (Just (1, (0b0000 :: P.BitVector 4) P.++# (30 :: P.BitVector 6) P.++# (0b0000_0000_0000_0001 :: P.BitVector 16) P.++# (0 :: P.BitVector 6) P.++# (20 :: P.BitVector 16) P.++# (0 :: P.BitVector 8) P.++# (0 :: P.BitVector 8)), Nothing) :: (Maybe (P.Unsigned InsnMemSizeBits, AluInsn), Maybe (P.Vec 2 AluToken)), -- 2
               -- LoadConst 22
               (Just (2, (0b0000 :: P.BitVector 4) P.++# (30 :: P.BitVector 6) P.++# (0b0000_0000_0000_0001 :: P.BitVector 16) P.++# (0 :: P.BitVector 6) P.++# (22 :: P.BitVector 16) P.++# (0 :: P.BitVector 8) P.++# (0 :: P.BitVector 8)), Nothing), -- 3
               -- Add
               (Just (3, (0b0001 :: P.BitVector 4) P.++# (30 :: P.BitVector 6) P.++# (0b0000_0000_0000_0010 :: P.BitVector 16) P.++# (0 :: P.BitVector 6) P.++# (0 :: P.BitVector 16) P.++# (0 :: P.BitVector 8) P.++# (1 :: P.BitVector 8)), Nothing), -- 4
               (Nothing, Just (AluToken 0 0 0 0 :> AluToken 0 0 0 0 :> Nil)), -- 5
               (Nothing, Just (AluToken 1 0 0 0 :> AluToken 0 0 0 0 :> Nil)), -- 6
               (Nothing, Just (AluToken 2 0 0 0 :> AluToken 0 0 0 0 :> Nil)), -- 7
               (Nothing, Just (AluToken 3 0 0 20 :> AluToken 0 0 0 22 :> Nil)), -- 8
               (Nothing, Nothing), -- 9
               (Nothing, Nothing), -- 10
               -- DelayOne
               (Just (4, (0b0001 :: P.BitVector 4) P.++# (30 :: P.BitVector 6) P.++# (0b0000_0000_0000_0010 :: P.BitVector 16) P.++# (0 :: P.BitVector 6) P.++# (0 :: P.BitVector 16) P.++# (0 :: P.BitVector 8) P.++# (2 :: P.BitVector 8)), Nothing), -- 11
               (Nothing, Just (AluToken 4 0 0 11 :> AluToken 0 0 0 0 :> Nil)), -- 12
               (Nothing, Nothing), -- 13
               (Nothing, Nothing), -- 14
               (Nothing, Nothing) -- 15
             ]
         )
    expectOutput =
      outputVerifier'
        clk
        rst
        $( P.listToVecTH
             [ (True, Nothing) :: (Bool, Maybe AluOutput), -- 1
               (True, Nothing), -- 2
               (True, Nothing), -- 3
               (True, Nothing), -- 4
               (True, Nothing), -- 5
               (True, Just $ AluOutput (AluToken 30 0 0 0) $(P.listToVecTH [True, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False])), -- 6
               (True, Just $ AluOutput (AluToken 30 0 0 20) $(P.listToVecTH [True, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False])), -- 7
               (True, Just $ AluOutput (AluToken 30 0 0 22) $(P.listToVecTH [True, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False])), -- 8
               (True, Just $ AluOutput (AluToken 30 0 0 42) $(P.listToVecTH [False, True, False, False, False, False, False, False, False, False, False, False, False, False, False, False])), -- 9
               (True, Nothing), -- 10
               (True, Nothing), -- 11
               (False, Nothing), -- 12
               (True, Nothing), -- 13
               (True, Just $ AluOutput (AluToken 30 0 0 11) $(P.listToVecTH [False, True, False, False, False, False, False, False, False, False, False, False, False, False, False, False])), -- 14
               (True, Nothing) -- 15
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
