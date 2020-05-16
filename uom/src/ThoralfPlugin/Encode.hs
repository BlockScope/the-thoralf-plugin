module ThoralfPlugin.Encode (thoralfTheories) where

import TcRnTypes (TcPluginM)
import ThoralfPlugin.Encode.TheoryEncoding
import ThoralfPlugin.Encode.UoM (uomTheory)

thoralfTheories :: TcPluginM TheoryEncoding
thoralfTheories = sumEncodings [uomTheory]
