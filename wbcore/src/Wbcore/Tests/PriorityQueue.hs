module Wbcore.Tests.PriorityQueue where

import Clash.Explicit.Testbench
import qualified Clash.Prelude as P
import qualified GHC.IO
import qualified Wbcore.Logic.PriorityQueue as PQ
import Wbcore.Types.Alias (Busy, busy, notBusy)
import Prelude

pqInst ::
  P.HiddenClockResetEnable dom =>
  P.Signal dom (Maybe Int, Bool) ->
  P.Signal dom (Maybe Int, Busy)
pqInst sigIn = P.bundle $ PQ.priorityQueue (P.SNat :: P.SNat 5) (<) push pop
  where
    (push, pop) = P.unbundle sigIn

topEntity ::
  P.Clock P.System ->
  P.Reset P.System ->
  P.Enable P.System ->
  P.Signal P.System (Maybe Int, Bool) ->
  P.Signal P.System (Maybe Int, Busy)
topEntity = P.exposeClockResetEnable pqInst

testBench :: P.Signal P.System Bool
testBench = done
  where
    testInput =
      stimuliGenerator
        clk
        rst
        $( P.listToVecTH
             [ -- (push, pop)

               -- Push 3 items
               (Just 3, False) :: (Maybe Int, Bool), -- 1
               (Just 2, False), -- 2
               (Just 5, False), -- 3

               -- Pop one
               (Nothing, True), -- 4

               -- Push until full
               (Just 1, False), -- 5
               (Just 9, False), -- 6
               (Just 2, False), -- 7
               (Just 11, False), -- 8

               -- Pop until empty
               (Nothing, True), -- 9
               (Nothing, True), -- 10
               (Nothing, True), -- 11
               (Nothing, True), -- 12
               (Nothing, True), -- 13
               (Nothing, False), -- 14
               (Nothing, True), -- 15
               (Nothing, False), -- 16

               -- Concurrently push + pop
               (Just 3, False), -- 17
               (Just 2, True), -- 18
               (Just 1, True), -- 19
               (Nothing, True), -- 20
               (Nothing, False) -- 21
             ]
         )
    expectOutput =
      outputVerifier'
        clk
        rst
        $( P.listToVecTH
             [ (Nothing, notBusy) :: (Maybe Int, Busy), -- 1
             -- Push 3 items
               (Just 3, notBusy), -- 2
               (Just 2, notBusy), -- 3
               (Just 2, notBusy), -- 4

               -- Pop one
               (Just 3, notBusy), -- 5

               -- Push until full
               (Just 1, notBusy), -- 6
               (Just 1, notBusy), -- 7
               (Just 1, busy), -- 8
               (Just 1, notBusy), -- 9

               -- Pop until empty
               (Just 2, notBusy), -- 10
               (Just 3, notBusy), -- 11
               (Just 5, notBusy), -- 12
               (Just 9, notBusy), -- 13
               (Just 11, notBusy), -- 14
               (Just 11, notBusy), -- 15
               (Nothing, notBusy), -- 16
               (Nothing, notBusy), -- 17

               -- Concurrently push + pop
               (Just 3, notBusy), -- 18
               (Just 2, notBusy), -- 19
               (Just 1, notBusy), -- 20
               (Nothing, notBusy) -- 21
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
