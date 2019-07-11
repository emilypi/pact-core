{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Pact.Syntax.Type
( -- * Data
  Type(..)
, TypeF(..)
, Kind(..)
, KindF(..)
, GuardType(..)
, Prim(..)
  -- * Traversals
, subtypes
, tyvars
  -- * Prisms
, _TyVar
, _TyFun
, _TyForall
, _TyLam
, _TyApp
, _TyGuard
, _TyRow
, _TyHole
, _KType
, _KArrow
, _KRow
, _KHole
, _TyInteger
, _TyDecimal
, _TyBool
, _TyString
, _TyTime
, _GTyKeySet
, _GTyKeySetName
, _GTyPact
, _GTyUser
, _GTyModule
) where

import GHC.Generics

import Control.DeepSeq
import Control.Lens

import Data.Functor.Foldable
import Data.Map as Map

data GuardType
  = GTyKeySet
  | GTyKeySetName
  | GTyPact
  | GTyUser
  | GTyModule
  deriving (Eq, Ord, Generic, Show, NFData)

makePrisms ''GuardType

data Prim
  = TyInteger
  | TyDecimal
  | TyTime
  | TyBool
  | TyString
  deriving (Eq, Ord, Show, Generic, NFData)

makePrisms ''Prim

data Kind a
  = KType a
    -- ^ The kind of concrete types (i.e. * or Type)
  | KArrow a (Kind a) (Kind a)
    -- ^ The kind of kind-level arrows Type -> Type
  | KRow a (Kind a)
    -- ^ The kind of row-types
  | KHole a Int
    -- ^ The kind of unknown kinds Kind : Kind - used as a unification kind
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, NFData, Generic)
makePrisms ''Kind

data KindF a x
  = KTypeF a
  | KArrowF a x x
  | KRowF a x
  | KHoleF a Int
  deriving (Functor, Traversable, Foldable)

type instance Base (Kind a) = KindF a

instance Recursive (Kind a) where
  project (KType a) = KTypeF a
  project (KArrow a dom cod) = KArrowF a dom cod
  project (KRow a row) = KRowF a row
  project (KHole a i) = KHoleF a i

instance Corecursive (Kind a) where
  embed (KTypeF a) = KType a
  embed (KArrowF a dom cod) = KArrow a dom cod
  embed (KRowF a row) = KRow a row
  embed (KHoleF a i) = KHole a i

type Label = String
type Module = String
type Signature = String

data Type name a
  = TyVar a (name a)
    -- ^ The type of single type variables
  | TyFun a (Type name a) (Type name a)
    -- ^ The type of function types
  | TyForall a (name a) (Kind a) (Type name a)
    -- ^ The type of type schema (forall a)
  | TyBuiltin a Prim
    -- ^ The type of builtin (primitive) types
  | TyLam a (name a) (Kind a) (Type name a)
    -- ^ The type of lambda abstractions
  | TyApp a (Type name a) (Type name a)
    -- ^ The type of β-reducible expressions
  | TyGuard a GuardType
    -- ^ The type of security primitives
  | TyRow a (Map Label (Type name a))
    -- ^ The type of non-empty types and their labels
  | TyHole a Int
    -- ^ Type : Type
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, NFData, Generic)

makePrisms ''Type

data TypeF name a x
  = TyBuiltinF a Prim
  | TyVarF a (name a)
  | TyFunF a x x
  | TyForallF a (name a) (Kind a) x
  | TyLamF a (name a) (Kind a) x
  | TyAppF a x x
  | TyGuardF a GuardType
  | TyRowF a (Map Label x)
  | TyHoleF a Int
  deriving (Functor, Traversable, Foldable)

type instance Base (Type name a) = TypeF name a

instance Recursive (Type name a) where
  project (TyVar a n) = TyVarF a n
  project (TyBuiltin a p) = TyBuiltinF a p
  project (TyFun a dom cod) = TyFunF a dom cod
  project (TyForall a n k t) = TyForallF a n k t
  project (TyLam a n k t) = TyLamF a n k t
  project (TyApp a t u) = TyAppF a t u
  project (TyGuard a g) = TyGuardF a g
  project (TyRow a m) = TyRowF a m
  project (TyHole a i) = TyHoleF a i

instance Corecursive (Type name a) where
  embed (TyVarF a n) = TyVar a n
  embed (TyBuiltinF a p) = TyBuiltin a p
  embed (TyFunF a dom cod) = TyFun a dom cod
  embed (TyForallF a n k t) = TyForall a n k t
  embed (TyLamF a n k t) = TyLam a n k t
  embed (TyAppF a t u) = TyApp a t u
  embed (TyGuardF a g) = TyGuard a g
  embed (TyRowF a m) = TyRow a m
  embed (TyHoleF a i) = TyHole a i

tyvars :: Traversal' (Type name a) (name a)
tyvars = _TyVar . traverse
{-# INLINABLE tyvars #-}

subtypes :: Traversal'(Type name a) (Type name a)
subtypes f = \case
  TyApp a t u -> TyFun a <$> f t <*> f u
  TyLam a n k t -> TyLam a n k <$> f t
  TyForall a n k t -> TyForall a n k <$> f t
  TyRow a m -> TyRow a <$> traverse f m
  TyFun a d c -> TyFun a <$> f d <*> f c
  t -> pure t
{-# INLINABLE subtypes #-}
