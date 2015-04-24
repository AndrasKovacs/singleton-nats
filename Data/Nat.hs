{-# LANGUAGE
  UndecidableInstances, ScopedTypeVariables, DataKinds,
  FlexibleInstances, GADTs, TypeFamilies, TemplateHaskell,
  TypeOperators #-}

module Data.Nat (
    Nat(..)
  , (Data.Nat.+)
  , (Data.Nat.*)
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

type family Lit n where
  Lit 0 = Z
  Lit n = S (Lit (n Lit.- 1))

type SLit n = Sing (Lit n)




