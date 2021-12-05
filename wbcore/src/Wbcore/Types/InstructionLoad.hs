module Wbcore.Types.InstructionLoad where

import Clash.Prelude
import Wbcore.Types.Alias
import Wbcore.Types.DispatchTag (DispatchTag)

data InstructionLoadBus = InstructionLoadBus
  { valid :: Bool
  , dispatchTag :: DispatchTag
  , instruction :: InstructionWord
  } deriving (Generic, NFDataX)
