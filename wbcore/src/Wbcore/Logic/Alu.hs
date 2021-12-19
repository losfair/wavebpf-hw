module Wbcore.Logic.Alu where

import Clash.Prelude
import Wbcore.Types.Alias (Busy, busy, notBusy)

data AluState = StIdle | StDelayOne AluOutput | StDelayTwo AluOutput
  deriving (Show, Eq, Generic, NFDataX)

type AluWord = BitVector 32

type AluInsn = BitVector 64

type InsnMemSizeBits = 6

type AluIndexBits = 4

type WaveCounterBits = 16

type WaveCounter = BitVector WaveCounterBits

data FullInsnAddr = FullInsnAddr
  { fia_aluIndex :: Unsigned AluIndexBits,
    fia_insnAddr :: Unsigned InsnMemSizeBits
  }
  deriving (Show, Eq, Generic, NFDataX)

data AluToken = AluToken
  { at_addr :: FullInsnAddr,
    at_wave :: WaveCounter,
    at_data :: AluWord
  }
  deriving (Show, Eq, Generic, NFDataX)

data AluOutput = AluOutput
  { ao_token :: AluToken,
    ao_mask :: Vec (2 ^ AluIndexBits) Bool
  }
  deriving (Show, Eq, Generic, NFDataX)

alu ::
  HiddenClockResetEnable dom =>
  Signal dom (Maybe (Unsigned InsnMemSizeBits, AluInsn)) ->
  Signal dom (Maybe (Vec 2 AluToken)) ->
  Signal dom (Bool, Maybe AluOutput)
alu refill input = bundle (wannaPop, stage2Out)
  where
    (busy_, stage1Out) = unbundle $ mealy aluDecode' (repeat 0) $ bundle (refill, input)
    stage2In_ = register Nothing stage1Out
    stage2In = (\(x, ignore) -> if ignore then Nothing else x) <$> bundle (stage2In_, didUnlock)
    (stage2Out, unlock) = unbundle $ mealy aluExec' StIdle stage2In
    wannaPop = (\(unlock_, b) -> unlock_ || b == notBusy) <$> bundle (unlock, busy_)
    didUnlock = register False unlock

-- busy = fmap (\x -> x && notBusy) $ bunbusy_

aluDecode' ::
  Vec (2 ^ InsnMemSizeBits) AluInsn ->
  (Maybe (Unsigned InsnMemSizeBits, AluInsn), Maybe (Vec 2 AluToken)) ->
  (Vec (2 ^ InsnMemSizeBits) AluInsn, (Busy, Maybe (AluInsn, Vec 2 AluToken)))
aluDecode' im (Just (i, insn), _) = (replace i insn im, (notBusy, Nothing))
aluDecode' im (Nothing, Nothing) = (im, (notBusy, Nothing))
aluDecode' im (Nothing, Just x) = (im, (if insnIsBlocking insn then busy else notBusy, Just (insn, x)))
  where
    insn = im !! fia_insnAddr (at_addr (head x))

aluExec' ::
  AluState ->
  Maybe (AluInsn, Vec 2 AluToken) ->
  -- (newSt, (output, unlock))
  (AluState, (Maybe AluOutput, Bool))
aluExec' StIdle Nothing = (StIdle, (Nothing, False))
aluExec' StIdle (Just (insn, x)) =
  case slice d7 d0 insn of
    1 -> (StIdle, (Just AluOutput {ao_token = AluToken {at_addr = insnNextAddr insn, at_wave = at_wave (head x), at_data = at_data (head x) + at_data (x !! (1 :: Int))}, ao_mask = insnOutputMask insn}, False))
    2 ->
      ( StDelayOne
          ( AluOutput {ao_token = AluToken {at_addr = insnNextAddr insn, at_wave = at_wave (head x), at_data = at_data (head x)}, ao_mask = insnOutputMask insn}
          ),
        (Nothing, False)
      )
    3 ->
      ( StDelayTwo
          ( AluOutput {ao_token = AluToken {at_addr = insnNextAddr insn, at_wave = at_wave (head x), at_data = at_data (head x)}, ao_mask = insnOutputMask insn}
          ),
        (Nothing, False)
      )
    _ -> (StIdle, (Just AluOutput {ao_token = AluToken {at_addr = insnNextAddr insn, at_wave = at_wave (head x), at_data = 0}, ao_mask = insnOutputMask insn}, False))
aluExec' (StDelayOne x) _ = (StIdle, (Just x, True))
aluExec' (StDelayTwo x) _ = (StDelayOne x, (Nothing, False))

insnIsBlocking :: AluInsn -> Bool
insnIsBlocking insn =
  case slice d7 d0 insn of
    2 -> True
    3 -> True
    _ -> False

insnNextAddr :: AluInsn -> FullInsnAddr
insnNextAddr insn =
  FullInsnAddr
    { fia_aluIndex = unpack $ slice d63 d60 insn,
      fia_insnAddr = unpack $ slice d59 d54 insn
    }

insnOutputMask :: AluInsn -> Vec (2 ^ AluIndexBits) Bool
insnOutputMask insn = map (\x -> testBit insn (38 + fromIntegral x)) indicesI
