module Wbcore.Tests.PrePe where

import Clash.Explicit.Testbench
import Clash.Prelude (Vec (Nil, (:>)))
import qualified Clash.Prelude as P
import qualified GHC.IO
import qualified Wbcore.Logic.PrePe as PrePe
import Wbcore.Types.Alias (Busy, busy, notBusy)
import Prelude

xInst ::
  P.HiddenClockResetEnable dom =>
  P.Signal dom (P.Vec 2 (P.Vec 3 (Maybe Int)), Bool) ->
  P.Signal dom (Maybe (P.Vec 2 Int), P.Vec 2 (P.Vec 3 Busy))
xInst sig = P.bundle (aggregatedQueue, P.bundle (P.map P.bundle frontendBusy))
  where
    (operands, backendWannaPop) = P.unbundle sig
    (aggregatedQueue, frontendBusy) = PrePe.tokenStream (P.SNat :: P.SNat 5) (==) (\x -> even x P.:> P.Nil) operands backendWannaPop

topEntity ::
  P.Clock P.System ->
  P.Reset P.System ->
  P.Enable P.System ->
  P.Signal P.System (P.Vec 2 (P.Vec 3 (Maybe Int)), Bool) ->
  P.Signal P.System (Maybe (P.Vec 2 Int), P.Vec 2 (P.Vec 3 Busy))
topEntity = P.exposeClockResetEnable xInst

testBench :: P.Signal P.System Bool
testBench = done
  where
    testInput =
      stimuliGenerator
        clk
        rst
        $( P.listToVecTH
             [ -- (P.Vec 2 (P.Vec 3 (Maybe Int)), Bool)
               -- ([[q0_a0, q0_a1, q0_a2], [q1_a0, q1_a1, q1_a2]], backendWannaPop)
               ((Nothing :> Nothing :> Nothing :> Nil) :> (Nothing :> Nothing :> Nothing :> Nil) :> Nil, False) :: (P.Vec 2 (P.Vec 3 (Maybe Int)), Bool), -- 1
               ((Just 2 :> Just 8 :> Nothing :> Nil) :> (Nothing :> Just 6 :> Just 2 :> Nil) :> Nil, False), -- 2
               ((Nothing :> Just 8 :> Nothing :> Nil) :> (Nothing :> Nothing :> Just 2 :> Nil) :> Nil, False), -- 3
               ((Nothing :> Nothing :> Nothing :> Nil) :> (Nothing :> Just 8 :> Nothing :> Nil) :> Nil, True), -- 4
               ((Nothing :> Nothing :> Nothing :> Nil) :> (Nothing :> Nothing :> Nothing :> Nil) :> Nil, False), -- 5
               ((Nothing :> Nothing :> Nothing :> Nil) :> (Nothing :> Nothing :> Nothing :> Nil) :> Nil, True), -- 6
               ((Nothing :> Nothing :> Nothing :> Nil) :> (Nothing :> Nothing :> Nothing :> Nil) :> Nil, True) -- 7
             ]
         )
    expectOutput =
      outputVerifier'
        clk
        rst
        $( P.listToVecTH
             [ -- (Maybe (P.Vec 2 Int), P.Vec 2 (P.Vec 3 Busy))
               -- (aggregatedQueue, frontendBusy)
               (Nothing, (notBusy :> notBusy :> notBusy :> Nil) :> (notBusy :> notBusy :> notBusy :> Nil) :> Nil) :: (Maybe (P.Vec 2 Int), P.Vec 2 (P.Vec 3 Busy)), -- 1
               (Nothing, (notBusy :> notBusy :> notBusy :> Nil) :> (notBusy :> notBusy :> notBusy :> Nil) :> Nil), -- 2
               (Nothing, (notBusy :> busy :> notBusy :> Nil) :> (notBusy :> notBusy :> busy :> Nil) :> Nil), -- 3
               (Just (2 :> 2 :> Nil), (notBusy :> notBusy :> notBusy :> Nil) :> (notBusy :> notBusy :> notBusy :> Nil) :> Nil), -- 4
               (Just (8 :> 8 :> Nil), (notBusy :> notBusy :> notBusy :> Nil) :> (notBusy :> notBusy :> notBusy :> Nil) :> Nil), -- 5
               (Just (8 :> 8 :> Nil), (notBusy :> notBusy :> notBusy :> Nil) :> (notBusy :> notBusy :> notBusy :> Nil) :> Nil), -- 6
               (Nothing, (notBusy :> notBusy :> notBusy :> Nil) :> (notBusy :> notBusy :> notBusy :> Nil) :> Nil) -- 7
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
