{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Pact.Syntax.Type
( -- * Data
  Type(..)
, Kind(..)
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
import Data.HashMap.Lazy as HashMap
import Data.List.NonEmpty as NonEmpty
import Data.Text


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
  | KConstraint a
    -- ^ The kind of functor constraints of modules
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, NFData, Generic)
makePrisms ''Kind

type Label = Text
type Module = Text
type Signature = Text

data Type a
  = TyVar a {-# UNPACK #-} !Text
    -- ^ The type of single type variables
  | TyForall a {-# UNPACK #-} !Text (Kind a) (Type a)
    -- ^ The type of type schema (forall a)
  | TyBuiltin a !Prim
    -- ^ The type of builtin (primitive) types
  | TyFun a {-# UNPACK #-} !Text (NonEmpty (Type a)) (Type a)
    -- ^ The type of function types and λ-abstractions
  | TyLam a (Kind a) (Type a)
    -- ^ The type of Λ-abstractions
  | TyApp a (Type a) (Type a)
    -- ^ The type of β-reducible expressions
  | TyGuard a !GuardType
    -- ^ The type of security primitives
  | TyRow a (HashMap  Text (Type a))
    -- ^ The type of non-empty types and their labels
  | TyProd a {-# UNPACK #-} !Int
    -- ^ The types of finite products
  | TyUnit a
    -- ^ The type of the terminal object in this category
  | TyHole a {-# UNPACK #-} !Int
    -- ^ The type of type Type. Used strictly for unification.
    -- n.b.: if we ever wnat pilinear types for capabilities, we need to
    -- discuss universe polymorphism. This needs cumulativity
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, NFData, Generic)

makePrisms ''Type

tyvars :: Traversal' (Type a) Text
tyvars = _TyVar . traverse
{-# INLINABLE tyvars #-}

subtypes :: Traversal' (Type a) (Type a)
subtypes f = \case
  TyApp a t u -> TyApp a <$> f t <*> f u
  TyLam a k t -> TyLam a k <$> f t
  TyForall a n k t -> TyForall a n k <$> f t
  TyRow a m -> TyRow a <$> traverse f m
  TyFun a n d c -> TyFun a n <$> (traverse f d) <*> f c
  t -> pure t
{-# INLINABLE subtypes #-}
