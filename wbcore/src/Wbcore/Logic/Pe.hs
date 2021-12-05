module Wbcore.Logic.Pe (pe, PeOutput, peMemReq, peTokenOut, TokenAcceptor) where

import Clash.Prelude
import Wbcore.Types.Alias
import Wbcore.Types.DispatchAcceptor (DispatchAcceptor, matchesDispatchTag, matchesIntraClusterDispatchTag)
import Wbcore.Types.DispatchTag (DispatchTag, IntraClusterDispatchTag (IntraClusterDispatchTag, slotIndex))
import qualified Wbcore.Types.DispatchTag
import Wbcore.Types.InstructionLoad (InstructionLoadBus)
import qualified Wbcore.Types.InstructionLoad
import qualified Wbcore.Types.Memory.Access
import Wbcore.Types.Token (Token (Token), TokenBody (TokenBody))

data PeOutput dom = PeOutput
  { peMemReq :: Signal dom Wbcore.Types.Memory.Access.Request,
    peTokenOut :: Signal dom TokenOut,
    peBusy :: Signal dom Busy
  }

type TokenOut = Vec TokenOutWidth (Maybe Token)

newtype InstrCache = InstrCache {ic_instr :: Vec PeSize InstructionWord} deriving (Show, Eq, Generic, NFDataX)

data TokenAcceptor = TokenAcceptor {ta_slotIndex :: SlotIndex, ta_body :: TokenBody} deriving (Show, Eq, Generic, NFDataX)

data DispatchStrategy = DispatchStrategy
  { ds_outPortMask :: BitVector TokenOutWidth,
    ds_dispatchTag :: DispatchTag,
    ds_waveCounter :: WaveCounter,
    ds_operandIndex :: Unsigned 1
  }
  deriving (Show, Eq, Generic, NFDataX)

data ExecuteState
  = EsIdle
  | EsMemWait DispatchStrategy
  deriving (Show, Eq, Generic, NFDataX)

data ExecuteTokenInput = ExecuteTokenInput
  { eti_ta :: Maybe TokenAcceptor,
    eti_instr_cache :: InstrCache,
    eti_mem_rsp :: Wbcore.Types.Memory.Access.Response
  }
  deriving (Show, Eq, Generic, NFDataX)

etiBundle ::
  HiddenClockResetEnable dom =>
  Signal dom (Maybe TokenAcceptor) ->
  Signal dom InstrCache ->
  Signal dom Wbcore.Types.Memory.Access.Response ->
  Signal dom ExecuteTokenInput
etiBundle ta instr_cache mem_rsp = ExecuteTokenInput <$> ta <*> instr_cache <*> mem_rsp

pe ::
  HiddenClockResetEnable dom =>
  DispatchAcceptor ->
  Signal dom (Maybe TokenAcceptor) ->
  Signal dom InstructionLoadBus ->
  Signal dom Wbcore.Types.Memory.Access.Response ->
  PeOutput dom
pe da ta instLoadBus memRsp = PeOutput {peMemReq = pure undefined, peTokenOut = tokenOut, peBusy = peBusy_}
  where
    instrCache = mealy (updateInstrCache da) (undefined :: InstrCache) instLoadBus
    (peBusy_, tokenOut) = unbundle $ mealy (computeBusy $ executeToken da) EsIdle $ etiBundle ta instrCache memRsp

dispatchDataWord :: DispatchStrategy -> DataWord -> TokenOut
dispatchDataWord DispatchStrategy {ds_outPortMask = opm_, ds_dispatchTag = dt_, ds_operandIndex = oi_, ds_waveCounter = wc_} dw = out
  where
    seq_ = iterate (SNat :: SNat TokenOutWidth) (+ 1) (0 :: Int)
    idt_ = Wbcore.Types.DispatchTag.intraCluster dt_
    out =
      map
        ( \i ->
            if testBit opm_ i
              then
                Just
                  ( Token
                      (Wbcore.Types.DispatchTag.clusterIndex dt_)
                      (Wbcore.Types.DispatchTag.peIndex idt_)
                      (TokenBody (Wbcore.Types.DispatchTag.slotIndex idt_) oi_ dw wc_)
                  )
              else Nothing
        )
        seq_

updateInstrCache :: DispatchAcceptor -> InstrCache -> InstructionLoadBus -> (InstrCache, InstrCache)
updateInstrCache da ic ilb = (next, ic)
  where
    next =
      if Wbcore.Types.InstructionLoad.valid ilb && matchesDispatchTag da dt
        then ic {ic_instr = replace (Wbcore.Types.DispatchTag.slotIndex idt) (Wbcore.Types.InstructionLoad.instruction ilb) (ic_instr ic)}
        else ic
    dt = Wbcore.Types.InstructionLoad.dispatchTag ilb
    idt = Wbcore.Types.DispatchTag.intraCluster dt

computeBusy :: (a -> b -> (ExecuteState, c)) -> a -> b -> (ExecuteState, (Busy, c))
computeBusy f a_ b_ = case f a_ b_ of
  (EsIdle, c_) -> (EsIdle, (notBusy, c_))
  (es, c_) -> (es, (busy, c_))

executeToken ::
  HiddenClockResetEnable dom =>
  DispatchAcceptor ->
  ExecuteState ->
  ExecuteTokenInput ->
  (ExecuteState, TokenOut)
executeToken _ EsIdle ExecuteTokenInput {eti_ta = Nothing} = (EsIdle, repeat Nothing)
executeToken _ EsIdle eti@ExecuteTokenInput {eti_ta = Just TokenAcceptor {ta_slotIndex = slot_index, ta_body = token_body}} = (es', tokenOut)
  where
    insn = ic_instr (eti_instr_cache eti) !! slot_index
    es' = EsIdle
    tokenOut = undefined
executeToken _ es@(EsMemWait _) ExecuteTokenInput {eti_mem_rsp = Wbcore.Types.Memory.Access.NoRsp} = (es, repeat Nothing)
executeToken da es@(EsMemWait _) ExecuteTokenInput {eti_mem_rsp = Wbcore.Types.Memory.Access.ReadResponse (idt, _)} | not (matchesIntraClusterDispatchTag da idt) = (es, repeat Nothing)
executeToken _ (EsMemWait ds) ExecuteTokenInput {eti_mem_rsp = Wbcore.Types.Memory.Access.ReadResponse (IntraClusterDispatchTag {slotIndex = slot_index}, w)} =
  (EsIdle, dispatchDataWord ds w)
