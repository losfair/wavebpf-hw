module Wbcore.Logic.PriorityQueue (priorityQueue, pqM) where

import Clash.Prelude
import Data.Maybe (isNothing)
import Wbcore.Types.Alias

priorityQueue ::
  HiddenClockResetEnable dom =>
  NFDataX a =>
  KnownNat n =>
  SNat n ->
  (a -> a -> Bool) ->
  Signal dom (Maybe a) ->
  Signal dom Bool ->
  (Signal dom (Maybe a), Signal dom Busy)
priorityQueue size lt push pop = unbundle $ mealy (pqM size lt) (repeat Nothing) (bundle (push, pop))

pqM ::
  KnownNat n =>
  SNat n ->
  (a -> a -> Bool) ->
  Vec (n + 1) (Maybe a) ->
  (Maybe a, Bool) ->
  (Vec (n + 1) (Maybe a), (Maybe a, Busy))
pqM _ lt st0 (pushInput, popReq) = (st2, (minElem, busy_))
  where
    (st1, minElem) = runPop lt st0 popReq
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
  (a -> a -> Bool) ->
  Vec (n + 1) (Maybe a) ->
  Bool ->
  (Vec (n + 1) (Maybe a), Maybe a)
runPop lt v pop = (newV, minElem)
  where
    minElemIndex = fold (foldOnce lt v) indicesI
    minElem = v !! minElemIndex
    newV = if pop then replace minElemIndex Nothing v else v

    foldOnce :: KnownNat n => (a -> a -> Bool) -> Vec (n + 1) (Maybe a) -> Index (n + 1) -> Index (n + 1) -> Index (n + 1)
    foldOnce lt_ v_ ia ib =
      case (a_, b_) of
        (Just a, Just b) -> if lt_ a b then ia else ib
        (Just _1, Nothing) -> ia
        (Nothing, Just _) -> ib
        (Nothing, Nothing) -> ia
      where
        a_ = v_ !! ia
        b_ = v_ !! ib

genPushMask_ :: KnownNat n => Vec (n + 1) (Maybe a) -> Int -> (Vec (n + 1) Bool, Index 3)
genPushMask_ v i
  | isNothing (v !! i) = (if cnt /= 0 then prev else replace i True prev, satAdd SatBound cnt 1)
  | otherwise = (prev, cnt)
  where
    (prev, cnt) =
      if i == 0 then (repeat False, 0) else genPushMask_ v (i - 1)

genPushMask :: KnownNat n => Vec (n + 1) (Maybe a) -> (Vec (n + 1) Bool, Index 3)
genPushMask v = genPushMask_ v (length v - 1)
