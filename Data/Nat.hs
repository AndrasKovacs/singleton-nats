{-# LANGUAGE
  UndecidableInstances, ScopedTypeVariables, DataKinds,
  FlexibleInstances, GADTs, TypeFamilies, TemplateHaskell,
  InstanceSigs, TypeOperators, PolyKinds #-}

module Data.Nat (
    Nat(..)
  , NatPlus
  , NatMul
  , NatMinus
  , NatAbs
  , natPlus
  , natMul
  , natMinus
  , natAbs
  , SNat
  , Data.Singletons.Prelude.Sing(SS, SZ)
  , Data.Singletons.Prelude.PNum
  , Data.Singletons.Prelude.SNum    
  , SSym0(..)
  , SSym1(..)
  , ZSym0(..)
  , Lit
  , SLit ) where

import Data.Singletons.TH
import Data.Singletons.Prelude
import Unsafe.Coerce
import qualified GHC.TypeLits as Lit  

$(singletons [d|
  data Nat = Z | S Nat deriving (Eq, Show, Ord)

  natPlus :: Nat -> Nat -> Nat
  natPlus Z     b = b
  natPlus (S a) b = S (natPlus a b)

  natMul :: Nat -> Nat -> Nat
  natMul Z     b = Z
  natMul (S a) b = natPlus b (natMul a b)

  natMinus :: Nat -> Nat -> Nat
  natMinus Z     b     = Z
  natMinus (S a) (S b) = natMinus a b
  natMinus a     Z     = a

  natAbs :: Nat -> Nat
  natAbs n = n
  |])                                

instance PNum ('KProxy :: KProxy Nat) where
  type a :+ b = NatPlus a b
  type a :- b = NatMinus a b
  type a :* b = NatMul a b
  type Abs a = NatAbs a
  type Signum (a :: Nat) = Error "Data.Nat: signum not implemented"
  type FromInteger (a :: Lit.Nat) = Lit a

instance SNum ('KProxy :: KProxy Nat) where  
  (%:+) = sNatPlus
  (%:*) = sNatMul
  (%:-) = sNatMinus
  sAbs  = sNatAbs  
  sSignum = case toSing "Data.Nat: signum not implemented" of
    SomeSing s -> sError s    
  sFromInteger n = case n %:== (sing :: Sing 0) of
    STrue  -> unsafeCoerce SZ
    SFalse -> unsafeCoerce (SS (sFromInteger (n %:- (sing :: Sing 1))))

{-| Converts a runtime 'Integer' to an existentially wrapped 'Nat'. Returns 'Nothing' if
the argument is negative -}
someNatVal :: Integer -> Maybe (SomeSing (KindOf Z))
someNatVal n = case Lit.someNatVal n of
  Just (Lit.SomeNat (pn :: Proxy n)) -> Just (SomeSing (sFromInteger (sing :: Sing n)))
  Nothing -> Nothing

{-| Provides a shorthand for 'Nat'-s using "GHC.TypeLits", for example:

>>> :kind! Lit 3
Lit 3 :: Nat
= 'S ('S ('S 'Z))
-}

type family Lit n where
  Lit 0 = Z
  Lit n = S (Lit (n Lit.- 1))

type SLit n = Sing (Lit n)

