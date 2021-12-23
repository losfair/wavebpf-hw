{-# LANGUAGE NumericUnderscores #-}

module Wbcore.Tests.Cluster where

import Clash.Explicit.Testbench
import Clash.Prelude (Vec (Nil, (:>)))
import qualified Clash.Prelude as P
import Debug.Trace (trace)
import qualified GHC.IO
import Wbcore.Logic.Alu (AluIndexBits, AluInsn, AluOutput (AluOutput), AluToken (AluToken), InsnMemSizeBits, alu)
import Wbcore.Logic.Cluster (cluster)
import Prelude

xInst ::
  P.HiddenClockResetEnable dom =>
  P.Signal dom (Maybe (P.Unsigned AluIndexBits, P.Unsigned InsnMemSizeBits, AluInsn), Maybe AluOutput) ->
  P.Signal dom (Maybe AluToken)
xInst x = cluster refill input
  where
    (refill, input) = P.unbundle x

topEntity ::
  P.Clock P.System ->
  P.Reset P.System ->
  P.Enable P.System ->
  P.Signal P.System (Maybe (P.Unsigned AluIndexBits, P.Unsigned InsnMemSizeBits, AluInsn), Maybe AluOutput) ->
  P.Signal P.System (Maybe AluToken)
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
               (Just (1, 0, (0b0000 :: P.BitVector 4) P.++# (0 :: P.BitVector 6) P.++# (0b0000_0000_0101_0000 :: P.BitVector 16) P.++# (0 :: P.BitVector 6) P.++# (0 :: P.BitVector 16) P.++# (0 :: P.BitVector 8) P.++# (0 :: P.BitVector 8)), Nothing) :: (Maybe (P.Unsigned AluIndexBits, P.Unsigned InsnMemSizeBits, AluInsn), Maybe AluOutput), -- 1
               -- LoadConst 20
               (Just (2, 0, (0b1000 :: P.BitVector 4) P.++# (0 :: P.BitVector 6) P.++# (0b0000_0001_0000_0000 :: P.BitVector 16) P.++# (0 :: P.BitVector 6) P.++# (20 :: P.BitVector 16) P.++# (0 :: P.BitVector 8) P.++# (0 :: P.BitVector 8)), Nothing), -- 2
               -- LoadConst 22
               (Just (3, 0, (0b0000 :: P.BitVector 4) P.++# (0 :: P.BitVector 6) P.++# (0b0000_0010_0000_0000 :: P.BitVector 16) P.++# (0 :: P.BitVector 6) P.++# (22 :: P.BitVector 16) P.++# (0 :: P.BitVector 8) P.++# (0 :: P.BitVector 8)), Nothing), -- 3
               -- Add
               (Just (4, 0, (0b0000 :: P.BitVector 4) P.++# (0 :: P.BitVector 6) P.++# (0b0000_0000_0000_0001 :: P.BitVector 16) P.++# (0 :: P.BitVector 6) P.++# (0 :: P.BitVector 16) P.++# (0 :: P.BitVector 8) P.++# (1 :: P.BitVector 8)), Nothing), -- 4
               (Nothing, Just $ AluOutput (AluToken 0 0 0 42) (P.replace (6 :: Int) True $ P.replace (4 :: Int) True (P.repeat False))), -- 5
               (Nothing, Nothing), -- 6
               (Nothing, Nothing), -- 7
               (Nothing, Nothing), -- 8
               (Nothing, Nothing), -- 9
               (Nothing, Nothing), -- 10
               (Nothing, Nothing), -- 11
               (Nothing, Nothing), -- 12
               (Nothing, Nothing), -- 13
               (Nothing, Nothing), -- 14
               (Nothing, Nothing), -- 15
               (Nothing, Nothing), -- 16
               (Nothing, Nothing), -- 17
               (Nothing, Nothing), -- 18
               (Nothing, Nothing), -- 19
               (Nothing, Nothing), -- 20
               (Nothing, Nothing), -- 21
               (Nothing, Nothing), -- 22
               (Nothing, Nothing) -- 23
             ]
         )
    expectOutput =
      outputVerifier'
        clk
        rst
        $( P.listToVecTH
             [ Nothing :: (Maybe AluToken), -- 1
               Nothing, -- 2
               Nothing, -- 3
               Nothing, -- 4
               Nothing, -- 5
               Nothing, -- 6
               Nothing, -- 7
               Nothing, -- 8
               Nothing, -- 9
               Nothing, -- 10
               Nothing, -- 11
               Just (AluToken 0 0 0 42), -- 12
               Nothing, -- 13
               Nothing, -- 14
               Nothing, -- 15
               Nothing, -- 16
               Nothing, -- 17
               Nothing, -- 18
               Nothing, -- 19
               Nothing, -- 20
               Nothing, -- 21
               Nothing, -- 22
               Nothing -- 23
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
