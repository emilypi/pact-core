{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Copyright :  (c) Emily Pillmore 2019-2019
-- License   :  BSD-2-Clause
-- Maintainer:  Emily Pillmore <emily@kadena.io>
-- Stability :  experimental
-- Portability: non-portable
--
-- The pact type system and its traversals
--
module Pact.Types
( -- * Data
  Type(..)
, GuardType(..)
, Prim(..)
, TypeSort(..)
  -- * Traversals
, subtypes
, tyvars
  -- * Prisms
, _TyVar
, _TyFun
, _TyForall
, _TyApp
, _TyGuard
, _TyRow
, _TyHole
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
import Data.Hashable
import Data.HashMap.Lazy as HashMap
import Data.List.NonEmpty as NonEmpty
import Data.Text
import Data.Word

import Pact.Kinds


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

data Type n a
  = TyVar a n
    -- ^ The type of single type variables
  | TyForall a n (Kind a) (Type n a)
    -- ^ The type of type schema (forall a)
  | TyBuiltin a !Prim
    -- ^ The type of builtin (primitive) types
  | TyFun a n (NonEmpty (Type n a)) (Type n a)
    -- ^ The type of function types and λ-abstractions
  | TyApp a (Type n a) (Type n a)
    -- ^ The type of β-reducible types
  | TyGuard a !GuardType
    -- ^ The type of security primitives TODO: pilinearity
  | TyRow a !(HashMap Text (Type n a))
    -- ^ The type of finite, heterogenous products (records)
    -- tuples are a trivial case of this where each label is π_n
    -- and each type is the same
  | TyStep a (Type n a) (Type n a) (Type n a)
    -- ^ the type of individual pact steps with return type,
    -- yield type, and resume types
  | TyUnit a
    -- ^ The type of the terminal object in this category
  | TyHole a {-# UNPACK #-} !Word64
    -- ^ The type of type Type. Used strictly for unification.
    -- n.b.: if we ever wnat pilinear types for capabilities, we need to
    -- discuss universe polymorphism. This needs cumulativity
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, NFData, Generic)
makePrisms ''Type


data TypeSort
  = Primitive
  | Security
  | Local
  | Scoped
  deriving (Eq, Ord, Show, Generic, NFData, Hashable)

tyvars :: Traversal' (Type n a) n
tyvars = _TyVar . traverse
{-# INLINABLE tyvars #-}

subtypes :: Traversal' (Type n a) (Type n a)
subtypes f = \case
  TyApp a t u -> TyApp a <$> f t <*> f u
  TyForall a n k t -> TyForall a n k <$> f t
  TyRow a m -> TyRow a <$> traverse f m
  TyFun a n d c -> TyFun a n <$> (traverse f d) <*> f c
  t -> pure t
{-# INLINABLE subtypes #-}
