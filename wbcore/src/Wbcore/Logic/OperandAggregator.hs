module Wbcore.Logic.OperandAggregator where

import Clash.Prelude
import Data.Maybe (fromJust, isJust)

operandAggregator ::
  HiddenClockResetEnable dom =>
  NFDataX a =>
  KnownNat n =>
  KnownNat m =>
  (a -> Vec m Bool) ->
  (a -> c) ->
  Signal dom (Vec n (Maybe a)) ->
  (Signal dom (Maybe (Vec m c)), Signal dom (Vec n Bool))
operandAggregator decode transform src = unbundle $ pureOperandAggregator decode transform <$> src

pureOperandAggregator ::
  KnownNat n =>
  KnownNat m =>
  (a -> Vec m Bool) ->
  (a -> c) ->
  Vec n (Maybe a) ->
  (Maybe (Vec m c), Vec n Bool)
pureOperandAggregator decode transform src =
  if allSatisfied
    then (Just $ transform . fromJust <$> satSig, popSig)
    else (Nothing, repeat False)
  where
    (popSig, satSig) =
      ifoldl
        ( \(pop, sat) i x ->
            if fmap isJust sat == repeat True
              then (pop, sat)
              else case x of
                Nothing -> (pop, sat)
                Just y -> (replace i True pop, broadcast sat (decode y) y)
        )
        -- (pop, satisfied)
        (repeat False, repeat Nothing)
        src
    allSatisfied = fmap isJust satSig == repeat True

broadcast :: Vec n (Maybe a) -> Vec n Bool -> a -> Vec n (Maybe a)
broadcast orig mask val = zipWith (\x y -> if y then Just val else x) orig mask
