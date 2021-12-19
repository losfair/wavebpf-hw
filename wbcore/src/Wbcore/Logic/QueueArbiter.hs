module Wbcore.Logic.QueueArbiter (queueArbiter) where

import Clash.Prelude
import Data.Maybe
import Wbcore.Types.Alias (Busy, busy, notBusy)

queueArbiter ::
  HiddenClockResetEnable dom =>
  NFDataX a =>
  KnownNat n =>
  Vec (n + 1) (Signal dom (Maybe a)) ->
  Signal dom Busy ->
  (Signal dom (Maybe a), Vec (n + 1) (Signal dom Busy))
queueArbiter workReq backendBusy = (firstReq, busyFb)
  where
    latchWorkReq = map (register Nothing) workReq
    busyFb = busyFeedback (map (fmap isJust) latchWorkReq) backendBusy
    firstReq = firstRequest workReq

busyFeedback ::
  (HiddenClockResetEnable dom) =>
  KnownNat n =>
  Vec (n + 1) (Signal dom Bool) ->
  Signal dom Busy ->
  Vec (n + 1) (Signal dom Busy)
busyFeedback workReq backendBusy = unbundle $ uncurry busyFeedback' <$> bundle (bundle workReq, backendBusy)

busyFeedback' ::
  KnownNat n =>
  Vec (n + 1) Bool ->
  Busy ->
  Vec (n + 1) Busy
busyFeedback' workReq backendBusy =
  if backendBusy == busy
    then repeat busy
    else case elemIndex True workReq of
      Nothing -> repeat notBusy
      Just i -> replace i notBusy $ map (\x -> if x then busy else notBusy) workReq

firstRequest ::
  (HiddenClockResetEnable dom) =>
  KnownNat n =>
  Vec (n + 1) (Signal dom (Maybe a)) ->
  Signal dom (Maybe a)
firstRequest = foldl (\acc_ x_ -> (\(acc, x) -> if isJust acc then acc else x) <$> bundle (acc_, x_)) (pure Nothing)
