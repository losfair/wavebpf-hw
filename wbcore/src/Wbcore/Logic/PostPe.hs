module Wbcore.Logic.PostPe where

import Clash.Prelude
import Data.Maybe
import Wbcore.Types.Alias (Busy (isBusy), busy, notBusy)

outputBuffer ::
  HiddenClockResetEnable dom =>
  NFDataX a =>
  KnownNat num_dests =>
  Signal dom (Vec num_dests Bool, (Maybe a, Vec num_dests Busy)) ->
  Signal dom (Vec num_dests (Maybe a), Busy)
outputBuffer = mealy outputBuffer' (repeat Nothing)

outputBuffer' ::
  KnownNat num_dests =>
  Vec num_dests (Maybe a) ->
  (Vec num_dests Bool, (Maybe a, Vec num_dests Busy)) ->
  (Vec num_dests (Maybe a), (Vec num_dests (Maybe a), Busy))
outputBuffer' st0 (outMask, (word, backendBusy)) = (st2, (st2, frontendBusy))
  where
    -- First, check completions...
    st1 = zipWith (\a b -> if isBusy b then a else Nothing) st0 backendBusy

    -- And give a busy signal to our frontend...
    frontendBusy = if isJust (findIndex isJust st1) then busy else notBusy

    -- Then, check if we have a new word to send...
    st2 =
      if isBusy frontendBusy
        then map (\x -> if x then word else Nothing) outMask
        else st0
