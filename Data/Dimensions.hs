{-# LANGUAGE TypeNaturals, ExistentialQuantification, FlexibleContexts,
             FlexibleInstances, GADTs, StandaloneDeriving #-}
module Data.Dimensions
  (
   Scale(..),
   Power(..),
   Quantity(..)
  )
  where

import GHC.TypeNats


data Whole
data Kilo
data Mega
data Giga
data Tera
data Peta
data Exa
data Zetta
data Yotta
data Milli
data Micro
data Nano
data Pico
data Femto
data Atto
data Zepto
data Yocto


class Scale scale
instance Scale Whole
instance Scale Mega
instance Scale Giga
instance Scale Tera
instance Scale Peta
instance Scale Exa
instance Scale Zetta
instance Scale Yotta
instance Scale Milli
instance Scale Micro
instance Scale Nano
instance Scale Pico
instance Scale Femto
instance Scale Atto
instance Scale Zepto
instance Scale Yocto


data Negative
data NonNegative

data PowerType :: * -> Nat -> *

class Power power
instance Power (PowerType NonNegative num)
instance (1 <= num) => Power (PowerType Negative num)


data Quantity scale power num where
  Quantity :: forall scale negativity power num
              . (Scale scale,
                 Power (PowerType negativity power),
                 Num num)
              => num
              -> Quantity scale (PowerType negativity power) num


deriving instance
  (Scale scale, Power (PowerType negativity power), Num num)
  => Eq (Quantity scale (PowerType negativity power) num)
