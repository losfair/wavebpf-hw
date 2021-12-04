module Wbcore.Cluster where

import Clash.Prelude

cluster :: HiddenClockResetEnable dom
  => KnownNat size
  =>
