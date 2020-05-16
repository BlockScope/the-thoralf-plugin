{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wunused-top-binds #-}

module ThoralfPlugin.Singletons.Symbol
  ( SSymbol (..),
    scomp,
  )
where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import ThoralfPlugin.Theory.DisEq
import Unsafe.Coerce

data SSymbol :: Symbol -> Type where
  SSym :: KnownSymbol s => SSymbol s

scomp :: SSymbol s -> SSymbol s' -> s :~?~: s'
scomp s@(SSym :: SSymbol s) s'@(SSym :: SSymbol s') =
  case symbolVal s == symbolVal s' of
    True -> unsafeCoerce Refl
    False -> forceCT @(DisEquality s s') DisRefl

-- Forcing Constraints

forceCT :: forall c x. (c => x) -> x
forceCT x = case unsafeCoerce (Dict :: Dict ()) :: Dict c of
  (Dict :: Dict c) -> x

data Dict :: Constraint -> Type where
  Dict :: a => Dict a
