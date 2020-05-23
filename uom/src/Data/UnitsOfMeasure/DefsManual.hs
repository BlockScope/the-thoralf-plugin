{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.UnitsOfMeasure.DefsManual where

import Data.Ratio ((%))
import Data.UnitsOfMeasure.Internal
import Data.UnitsOfMeasure.Convert

    {-
type instance MkUoM "m" = Base "m" 1
instance (IsCanonical (Unpack (Base "m" 1)), (Base "m" 1 /: Base "m" 1) ~ One) => HasCanonicalBaseUnit "m"

type instance MkUoM "s" = Base "s" 1
instance (IsCanonical (Unpack (Base "s" 1)), (Base "s" 1 /: Base "s" 1) ~ One) => HasCanonicalBaseUnit "s"

type instance MkUoM "km" = Base "m" 1
instance IsCanonical (Unpack (Base "m" 1)) => HasCanonicalBaseUnit "km" where
    type CanonicalBaseUnit "km" = Base "m" 1
    conversionBase _ = MkQuantity $ 1 % 1000

type instance MkUoM "kg" = Base "kg" 1
instance (IsCanonical (Unpack (Base "kg" 1)), (Base "kg" 1 /: Base "kg" 1) ~ One) => HasCanonicalBaseUnit "kg"

type instance MkUoM "g" = Base "kg" 1
instance IsCanonical (Unpack (Base "kg" 1)) => HasCanonicalBaseUnit "g" where
    type CanonicalBaseUnit "g" = Base "kg" 1
    conversionBase _ = MkQuantity $ 1000 % 1
    -}


