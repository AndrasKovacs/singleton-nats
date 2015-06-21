{-# LANGUAGE
  UndecidableInstances, ScopedTypeVariables, DataKinds,
  FlexibleInstances, GADTs, TypeFamilies, TemplateHaskell,
  TypeOperators #-}

module Data.Nat (
    Nat(..)
  , natPlus
  , natMul
  , SNat
  , (:*)
  , (:*$)
  , (:*$$)
  , (:+)    
  , (:+$)
  , (:+$$)
  , SSym0(..)
  , SSym1(..)
  , ZSym0(..)
  , (%:+)
  , (%:*)
  , Lit
  , SLit ) where

import Data.Singletons.TH
import Data.Singletons.Prelude
import qualified GHC.TypeLits as Lit

$(singletons [d|
  data Nat = Z | S Nat deriving (Eq, Show, Ord)

  (+) :: Nat -> Nat -> Nat
  Z + b = b
  S a + b = S (a + b)

  (*) :: Nat -> Nat -> Nat
  Z * b = Z
  S a * b = b + (a * b) |])

{-| This is the plain value-level version of addition on Nats. There's rarely a reason to use this;
it's included for completeness. -}
natPlus :: Nat -> Nat -> Nat
natPlus = (Data.Nat.+)

{-| Similarly to 'natPlus', this one is included for completeness. -}
natMul :: Nat -> Nat -> Nat
natMul = (Data.Nat.*)

{-| Provides a shorthand for 'Nat'-s using "GHC.TypeLits", for example:

>>> :kind! Lit 3
Lit 3 :: Nat
= 'S ('S ('S 'Z))
-}

type family Lit n where
  Lit 0 = Z
  Lit n = S (Lit (n Lit.- 1))

type SLit n = Sing (Lit n)

