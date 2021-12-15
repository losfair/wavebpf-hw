module Wbcore.Logic.FlatQueueAggregator where

import Clash.Prelude
import Data.Maybe (fromJust, isJust)

data IndexStrategy n = UseIndex (Index n) | DontNeed

aggregate ::
  HiddenClockResetEnable dom =>
  KnownNat n =>
  KnownNat m =>
  (a -> a -> Bool) ->
  (a -> Vec m Bool) ->
  Signal dom (Vec (m + 1) (Vec (n + 1) (Maybe a))) ->
  Signal dom (Vec (m + 1) (Maybe (Index (n + 1))), Maybe (Vec (m + 1) a))
aggregate eq satMask = fmap (pureAggregate eq satMask)

pureAggregate ::
  KnownNat n =>
  KnownNat m =>
  (a -> a -> Bool) ->
  (a -> Vec m Bool) ->
  Vec (m + 1) (Vec (n + 1) (Maybe a)) ->
  (Vec (m + 1) (Maybe (Index (n + 1))), Maybe (Vec (m + 1) a))
pureAggregate eq satMask input =
  (popSignals, outputValue)
  where
    -- For each element in the first queue, aggregate indices from all queues such that:
    -- - Each element at the to-be-aggregated index matches the first element's wave tag.
    -- - The aggregated set satisfies `satMask`.
    inputHead = head input
    inputTail = drop (SNat :: SNat 1) input
    aggOut = map (agg1 eq satMask inputTail) inputHead

    -- The first index in the first queue that can be popped.
    firstIndex = findIndex isJust aggOut
    firstMatchingValue = fmap (\x -> fromJust (inputHead !! x)) firstIndex

    -- Matching indices in the other queues.
    matchingIndices = fmap (\i -> fromJust (aggOut !! i)) firstIndex

    -- Matching values in the other queues.
    matchingValues =
      zipWith
        ( \a b -> case b of
            DontNeed -> undefined
            UseIndex i -> fromJust (a !! i)
        )
        inputTail
        <$> matchingIndices

    outputValue = fmap (\x -> (fromJust firstMatchingValue :> Nil) ++ x) matchingValues

    -- Which queues should receive a pop signal?
    popSignals = case matchingIndices of
      Just i ->
        firstIndex :> Nil
          ++ map
            ( \case
                DontNeed -> Nothing
                UseIndex j -> Just j
            )
            i
      Nothing -> replicate (lengthS input) Nothing

agg1 ::
  KnownNat n =>
  KnownNat m =>
  (a -> a -> Bool) ->
  (a -> Vec m Bool) ->
  Vec m (Vec (n + 1) (Maybe a)) ->
  Maybe a ->
  Maybe (Vec m (IndexStrategy (n + 1)))
agg1 _ _ _ Nothing = Nothing
agg1 eq satMask input (Just v) =
  if allMatch
    then Just $ map fromJust matchOut
    else Nothing
  where
    satMask_ = satMask v
    matches =
      map
        ( findIndex
            ( \case
                Just x_ -> eq v x_
                _ -> False
            )
        )
        input
    matchOut =
      zipWith
        ( \need i ->
            if not need
              then Just DontNeed
              else case i of
                Just i_ -> Just $ UseIndex i_
                Nothing -> Nothing
        )
        satMask_
        matches
    allMatch = foldl (\a b -> a && isJust b) True matchOut
