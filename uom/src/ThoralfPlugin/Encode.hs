module ThoralfPlugin.Encode (thoralfTheories) where

import TcRnTypes (TcPluginM)
import ThoralfPlugin.Encode.Bool (boolTheory)
import ThoralfPlugin.Encode.FiniteMap (fmTheory)
import ThoralfPlugin.Encode.Nat (natTheory)
import ThoralfPlugin.Encode.Symbol (symbolTheory)
import ThoralfPlugin.Encode.TheoryEncoding
import ThoralfPlugin.Encode.UoM (uomTheory)

thoralfTheories :: TcPluginM TheoryEncoding
thoralfTheories =
  sumEncodings
    [ natTheory,
      fmTheory,
      symbolTheory,
      boolTheory,
      uomTheory
    ]
