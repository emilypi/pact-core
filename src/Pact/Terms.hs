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
import Pact.Names
import Pact.Types hiding (subtypes)



-- | Is it a Row, Schema, or Table metaphor?
--
data RowSort = Object | Schema | Table
  deriving (Eq, Ord, Show, Generic, NFData, Hashable)

data PactStep term typ n a = PactStep
  { _stepName :: Maybe Text
    -- ^ If labeled, the step name (note: private pacts only)
  , _stepNumber :: {-# UNPACK #-} !Word64
    -- ^ step number: uints starting at 0
  , _stepBody :: term n a
    -- ^ the step body expression
  , _stepYield :: term n a
    -- ^ yield data, if present
  , _stepResume :: term n a
    -- ^ resume data, if present (must resume on previous step)
  , _stepReturnType :: typ n a
    -- ^ return type spec for return from pact step
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic, NFData)
makeLenses ''PactStep

data PactSort = Public | Private
  deriving (Show, Eq, Generic, NFData)

data Term n a
  = Var a n
    -- ^ named variable
  | Let a n (Type n a) (Term n a)
    -- ^ let bindings. Note: 'let x:y = m in n' desugars to
    -- (\x:y -> n) m, hence we can just make use of fun and app
  | App a (Term n a) (NonEmpty (Term n a))
    -- ^ β-reduction
  | Fun a n (Type n a) (Term n a)
    -- ^ function terms
  | Lit a (Literal a)
    -- ^ constant terms
  | Annot a (Term n a) (Type n a)
    -- ^ type annotation
  | Row a !RowSort (Type n a) (HashMap Text (Term n a))
    -- ^ row terms as used in bindings, schema/table decls
    -- In practice there is a semantic difference between declaring a row
    -- and binding var names to row entries
  | Step a !PactSort (PactStep Term Type n a)
    -- ^ step terms
  | Sequence a (NonEmpty (Term n a))
    -- ^ sequences of
  | Error a (Type n a)
    -- ^ the type of error terms
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic, NFData)
makePrisms ''Term

subterms :: Traversal' (Term n a) (Term n a)
subterms f = \case
  App a t u -> App a <$> f t <*> traverse f u
  Let a n ty t -> Let a n ty <$> f t
  Annot a t ty -> (\t' -> Annot a t' ty) <$> f t
  Fun a n ty t -> Fun a n ty <$> f t
  t -> pure t
{-# INLINABLE subterms #-}

subtypes :: Traversal' (Term n a) (Type n a)
subtypes f = \case
  Let a n ty t -> (\ty' -> Let a n ty' t) <$> f ty
  Annot a t ty -> Annot a t <$> f ty
  Error a ty -> Error a <$> f ty
  Fun a n ty t -> (\ty' -> Fun a n ty' t) <$> f ty
  t -> pure t
{-# INLINABLE subtypes #-}

vars :: Traversal' (Term n a) n
vars = _Var . traverse
{-# INLINABLE vars #-}

-- Patterns for row types
pattern DefSchema a ty m <- Row a Schema ty m
pattern DefTable a ty m <- Row a Table ty m
