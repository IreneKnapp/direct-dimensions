{-# LANGUAGE TypeNaturals, ExistentialQuantification, FlexibleContexts,
             FlexibleInstances, GADTs, StandaloneDeriving, TypeFamilies #-}
module Data.Dimensions
  (
   IntClass(..),
   IntType,
   Quantity(..)
  )
  where

import GHC.TypeNats


data IntType :: * -> Nat -> *
data Negative
data NonNegative
class IntClass int
instance IntClass (IntType NonNegative num)
instance (1 <= num) => IntClass (IntType Negative num)


type family PlusInt a b
type instance PlusInt (IntType NonNegative a) (IntType NonNegative b) =
  IntType NonNegative (a + b)
type instance PlusInt (IntType Negative a) (IntType Negative b) =
  IntType Negative (a + b)
type instance (a + b ~ ab)
              => PlusInt (IntType NonNegative ab)
                         (IntType Negative b) =
  IntType NonNegative a
type instance (a + b ~ ab)
              => PlusInt (IntType NonNegative a)
                         (IntType Negative ab) =
  IntType Negative b
type instance (a + b ~ ab)
              => PlusInt (IntType Negative ab)
                         (IntType NonNegative b) =
  IntType Negative a
type instance (a + b ~ ab)
              => PlusInt (IntType Negative a)
                         (IntType NonNegative ab) =
  IntType NonNegative b


type Whole = IntType NonNegative 0
type Kilo = IntType NonNegative 3
type Mega = IntType NonNegative 6
type Giga = IntType NonNegative 9
type Tera = IntType NonNegative 12
type Peta = IntType NonNegative 15
type Exa = IntType NonNegative 18
type Zetta = IntType NonNegative 21
type Yotta = IntType NonNegative 24
type Milli = IntType Negative 3
type Micro = IntType Negative 6
type Nano = IntType Negative 9
type Pico = IntType Negative 12
type Femto = IntType Negative 15
type Atto = IntType Negative 18
type Zepto = IntType Negative 21
type Yocto = IntType Negative 24


data Quantity scale int num where
  Quantity :: forall scale power num
              . (IntClass scale,
                 IntClass power,
                 Num num)
              => num
              -> Quantity scale power num


deriving instance
  (IntClass scale, IntClass power, Num num)
  => Eq (Quantity scale power num)
