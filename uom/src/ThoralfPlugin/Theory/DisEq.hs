{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

module ThoralfPlugin.Theory.DisEq
  ( DisEquality,
    (:~?~:) (..),
  )
where

--import Data.Kind ( Constraint )

class DisEquality (x :: k) (y :: k)

data a :~?~: b where
  Refl :: a :~?~: a
  DisRefl :: DisEquality a b => a :~?~: b
