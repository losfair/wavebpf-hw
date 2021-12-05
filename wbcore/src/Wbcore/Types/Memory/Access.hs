module Wbcore.Types.Memory.Access where

import Clash.Prelude
import Wbcore.Types.Alias
import Wbcore.Types.DispatchTag (IntraClusterDispatchTag)

data Request
  = NoReq
  | Read (IntraClusterDispatchTag, MemoryAddress)
  | Write (IntraClusterDispatchTag, MemoryAddress, DataWord)
  deriving (Show, Eq, Generic, NFDataX)

data Response
  = NoRsp
  | ReadResponse (IntraClusterDispatchTag, DataWord)
  deriving (Show, Eq, Generic, NFDataX)
