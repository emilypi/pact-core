{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Copyright :  (c) Emily Pillmore 2019-2019
-- License   :  BSD-2-Clause
-- Maintainer:  Emily Pillmore <emily@kadena.io>
-- Stability :  experimental
-- Portability: non-portable
--
-- The pact term language and its traversals
--
module Pact.Terms
( -- * Data
  Term(..)
, RowSort(..)
  -- * Traversals
, subtypes
, subterms
, vars
  -- * Prisms
, _Var
, _Let
, _App
, _Lit
, _Fun
, _Annot
, _Row
, _Error
  -- * Patterns
, pattern DefCap
, pattern DefNative
, pattern Defun
, pattern DefTable
, pattern DefSchema
) where


import GHC.Generics

import Control.DeepSeq
import Control.Lens

import Data.Functor.Foldable
import Data.Hashable
import Data.HashMap.Lazy
import Data.List.NonEmpty as NonEmpty
import Data.Text
import Data.Word

import Pact.AST.Literals
import Pact.Types hiding (subtypes)


-- | Is it a Row, Schema, or Table metaphor?
--
data RowSort = Object | Schema | Table
  deriving (Eq, Ord, Show, Generic, NFData, Hashable)

-- | Differentiate between the types of function terms
data FunPosition
  = Native
  | Capability
  | User
  | Pact
  deriving (Eq, Show, Generic, NFData, Hashable)

data Term a
  = Var a {-# UNPACK #-} !Text
    -- ^ named variable
  | Let a {-# UNPACK #-} !Text (Type a) (Term a)
    -- ^ let bindings. Note: 'let x:y = m in n' desugars to
    -- (\x:y -> n) m, hence we can just make use of fun and app
  | App a (NonEmpty (Term a)) (Term a)
    -- ^ β-reduction
  | Fun a !FunPosition (Type a) (Term a)
    -- ^ function terms
  | Lit a (Literal a)
    -- ^ constant terms
  | Annot a (Term a) (Type a)
    -- ^ type annotation
  | Row a !RowSort (Type a) (HashMap Text (Term a))
    -- ^ row terms as used in bindings, schema/table decls
    -- In practice there is a semantic difference between declaring a row
    -- and binding var names to row entries
  | Sig a (Type a)
    -- ^ signature terms representing function types with empty bodies
  | Error a (Type a)
    -- ^ the type of error terms
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic, NFData)
makePrisms ''Term

subterms :: Traversal' (Term a) (Term a)
subterms f = \case
  App a t u -> App a <$> (traverse f t) <*> f u
  Let a n ty t -> Let a n ty <$> f t
  Annot a t ty -> (\t' -> Annot a t' ty) <$> f t
  Fun a p ty t -> Fun a p ty <$> f t
  t -> pure t
{-# INLINABLE subterms #-}

subtypes :: Traversal' (Term a) (Type a)
subtypes f = \case
  Let a n ty t -> (\ty' -> Let a n ty' t) <$> f ty
  Annot a t ty -> Annot a t <$> f ty
  Error a ty -> Error a <$> f ty
  Fun a p ty t -> (\ty' -> Fun a p ty' t) <$> f ty
  Sig a ty -> Sig a <$> f ty
  t -> pure t

{-# INLINABLE subtypes #-}

vars :: Traversal' (Term a) Text
vars = _Var . traverse
{-# INLINABLE vars #-}

-- Patterns for function types
pattern DefCap a ty t <- Fun a Capability ty t
pattern Defun a ty t <- Fun a User ty t
pattern DefNative a ty t <- Fun a Native ty t
pattern DefPact a ty t <- Fun a Pact ty t

-- Patterns for row types
pattern DefSchema a ty m <- Row a Schema ty m
pattern DefTable a ty m <- Row a Table ty m
