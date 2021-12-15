module Wbcore.Logic.FlatQueue (flatQueue, fqM) where

import Clash.Prelude
import Data.Maybe (isNothing)
import Wbcore.Types.Alias

flatQueue ::
  HiddenClockResetEnable dom =>
  NFDataX a =>
  KnownNat n =>
  SNat n ->
  Signal dom (Maybe a) ->
  Signal dom (Maybe (Index (n + 1))) ->
  (Signal dom (Vec (n + 1) (Maybe a)), Signal dom Busy)
flatQueue size push pop = unbundle $ mealy (fqM size) (repeat Nothing) (bundle (push, pop))

fqM ::
  KnownNat n =>
  SNat n ->
  Vec (n + 1) (Maybe a) ->
  (Maybe a, Maybe (Index (n + 1))) ->
  (Vec (n + 1) (Maybe a), (Vec (n + 1) (Maybe a), Busy))
fqM _ st0 (pushInput, popReq) = (st2, (st0, busy_))
  where
    st1 = runPop st0 popReq
    (st2, busy_) = runPush st1 pushInput

runPush ::
  KnownNat n =>
  Vec (n + 1) (Maybe a) ->
  Maybe a ->
  (Vec (n + 1) (Maybe a), Busy)
runPush v maybeInput =
  case maybeInput of
    Just newItem ->
      ( zipWith (\a_ b_ -> if a_ then Just newItem else b_) pushMask v,
        Busy (emptySlotCountBeforePush == 0 || emptySlotCountBeforePush == 1)
      )
    Nothing -> (v, Busy (emptySlotCountBeforePush == 0))
  where
    (pushMask, emptySlotCountBeforePush) = genPushMask v

runPop ::
  KnownNat n =>
  Vec (n + 1) (Maybe a) ->
  Maybe (Index (n + 1)) ->
  Vec (n + 1) (Maybe a)
runPop v pop = newV
  where
    newV = case pop of
      Just i -> replace i Nothing v
      Nothing -> v

genPushMask_ :: KnownNat n => Vec (n + 1) (Maybe a) -> Int -> (Vec (n + 1) Bool, Index 3)
genPushMask_ v i
  | isNothing (v !! i) = (if cnt /= 0 then prev else replace i True prev, satAdd SatBound cnt 1)
  | otherwise = (prev, cnt)
  where
    (prev, cnt) =
      if i == 0 then (repeat False, 0) else genPushMask_ v (i - 1)

genPushMask :: KnownNat n => Vec (n + 1) (Maybe a) -> (Vec (n + 1) Bool, Index 3)
genPushMask v = genPushMask_ v (length v - 1)
