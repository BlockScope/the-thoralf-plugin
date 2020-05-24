{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
In this module the unit of measure interface is declared.
Really, this is just an abelian group isomorphic to Z+.
-}
module ThoralfPlugin.Theory.UoM
    ( UoM
    , FromList
    , One
    , IsBase
    , IsPow
    , IsProd
    , IsDiv
    ) where

import GHC.Types (Symbol)
import Data.Kind (Constraint)
import GHC.TypeLits

data UoM where {}

type family One :: UoM where {}
type family Base (measure :: Symbol) :: UoM where {}
type family Pow (measure :: Symbol) (power :: Nat) :: UoM where {}
type family (*:) (a :: UoM) (b :: UoM) :: UoM where {}
type family (/:) (n :: UoM) (d :: UoM) :: UoM where {}

type family IsBase (measure :: Symbol) (b :: UoM) :: Constraint where
  IsBase m b = (b ~ (Base m))

type family IsPow (measure :: Symbol) (power :: Nat) (b :: UoM) :: Constraint where
  IsPow m i b = (b ~ (Pow m i))

type family IsProd (a :: UoM) (b :: UoM) (aTimesb :: UoM) :: Constraint where
  IsProd a b c = (c ~ (a *: b))

type family IsDiv (a :: UoM) (b :: UoM) (aDivb :: UoM) :: Constraint where
  IsDiv a b c = (c ~ (a /: b))

type family FromList (xs :: [(Symbol,Nat)]) :: UoM where
  FromList '[] = One
  FromList ('(u, 1) ': ys) = (Base u) *: (FromList ys)
  FromList ('(u, i) ': ys) = (Pow u i) *: (FromList ys)
