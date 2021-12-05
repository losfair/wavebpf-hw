module Wbcore.Types.Alias where

import Clash.Prelude

type PeIndexBits = 5

type SlotIndexBits = 5

type TokenOutWidthBits = 1

type WaveCounterBits = 10

type DataWordBits = 32

type ClusterIndex = Unsigned 8

type PeIndex = Unsigned PeIndexBits

type SlotIndex = Unsigned SlotIndexBits

type InstructionWord = BitVector 64

type DataWord = BitVector DataWordBits

type MemoryAddress = Unsigned 32

type WaveCounter = Unsigned WaveCounterBits

type ClusterSize = 2 ^ PeIndexBits

type PeSize = 2 ^ SlotIndexBits

type TokenOutWidth = 2 ^ TokenOutWidthBits

newtype Busy = Busy {isBusy :: Bool}

isNotBusy :: Busy -> Bool
isNotBusy = not . isBusy

busy :: Busy
busy = Busy {isBusy = True}

notBusy :: Busy
notBusy = Busy {isBusy = False}

waveCounterLt :: WaveCounter -> WaveCounter -> Bool
waveCounterLt a b = b - a < a - b
