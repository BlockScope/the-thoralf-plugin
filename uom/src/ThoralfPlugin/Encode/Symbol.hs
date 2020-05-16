{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

module ThoralfPlugin.Encode.Symbol
  ( symbolTheory,
  )
where

import FastString (unpackFS)
import TcPluginM (TcPluginM)
import ThoralfPlugin.Encode.TheoryEncoding
import Type
  ( Type,
    isStrLitTy,
    splitTyConApp_maybe,
  )
import TysWiredIn (typeSymbolKindCon)

symbolTheory :: TcPluginM TheoryEncoding
symbolTheory = return symbolEncoding

symbolEncoding :: TheoryEncoding
symbolEncoding =
  emptyTheory
    { typeConvs = [symLitConv],
      kindConvs = [symKindConv]
    }

symLitConv :: Type -> Maybe TyConvCont
symLitConv ty = do
  fastStr <- isStrLitTy ty
  let str = unpackFS fastStr
  let sexprStr = "\"" ++ str ++ "\""
  return $
    TyConvCont VNil VNil ((const . const) sexprStr) []

symKindConv :: Type -> Maybe KdConvCont
symKindConv ty = do
  (tcon, _) <- splitTyConApp_maybe ty
  case tcon == typeSymbolKindCon of
    False -> Nothing
    True ->
      Just $ KdConvCont VNil (const "String")
