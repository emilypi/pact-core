{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module Pact.Syntax.Term
( -- * Data
  Builtin(..)
, Constant(..)
, Term(..)
, TermF(..)
  -- * Traversals
, subtypes
, subterms
, vars
  -- * Prisms
, _Var
, _Let
, _App
, _Constant
, _Annot
, _Row
, _Error
) where

import GHC.Generics

import Control.DeepSeq
import Control.Lens

import Data.Functor.Foldable
import Data.HashMap.Lazy
import Data.Text

import Pact.Syntax.Type hiding (subtypes)


data Builtin a = BuiltinName a String
  deriving (Eq, Show, Functor, Generic, NFData)

-- | Is it a Schema or Table metaphor?
data RowPosition = Schema | Table
  deriving (Eq, Ord, Show, Generic, NFData)

type Decimal = Double
type Time = Double

data Constant a
  = BuiltInInteger a Integer
  | BuiltInDecimal a Decimal
  | BuiltInString a {-# UNPACK #-} !Text
  | BuiltInTime a Time
  deriving (Eq, Show, Functor, Generic, NFData)

data Term tyname name a
  = Var a (name a)
    -- ^ named variable
  | Let a (name a) (Type tyname a) (Term tyname name a)
    -- ^ let bindings. Note: 'let x:y = m in n' desugars to
    -- (\x:y -> n) m, hence we just make use of lam and app
  | App a (Term tyname name a) (Term tyname name a)
    -- ^ β-reduction
  | Constant a (Constant a)
    -- ^ constant terms
  | Builtin a (Builtin a)
    -- ^ builtin terms
  | Annot a (Term tyname name a) (Type tyname a)
    -- ^ type annotation
  | Row a RowPosition (Type tyname a) (HashMap Text (Term tyname name a))
    -- ^ row terms as used in bindings, schema/table decls
    -- In practice there is a semantic difference between declaring a row
    -- and binding var names to row entries
  | Error a (Type tyname a)
    -- ^ the type of error terms
  deriving (Show, Functor, Generic, NFData)

makePrisms ''Term

data TermF tyname name a x
  = VarF a (name a)
  | LetF a (name a) (Type tyname a) x
  | AppF a x x
  | RowF a RowPosition (Type tyname a) (HashMap Text x)
  | ConstantF a (Constant a)
  | BuiltinF a (Builtin a)
  | AnnotF a x (Type tyname a)
  | ErrorF a (Type tyname a)
  deriving (Show, Functor, Generic, NFData)

type instance Base (Term tyname name a) = TermF tyname name a

instance Recursive (Term tyname name a) where
  project (Var a n) = VarF a n
  project (Let a n ty t) = LetF a n ty t
  project (App a t u) = AppF a t u
  project (Constant a c) = ConstantF a c
  project (Builtin a b) = BuiltinF a b
  project (Row a p ty m) = RowF a p ty m
  project (Annot a t ty) = AnnotF a t ty
  project (Error a ty) = ErrorF a ty

instance Corecursive (Term tyname name a) where
  embed (VarF a n) = Var a n
  embed (LetF a n ty t) = Let a n ty t
  embed (AppF a t u) = App a t u
  embed (ConstantF a c) = Constant a c
  embed (BuiltinF a b) = Builtin a b
  embed (RowF a p ty m) = Row a p ty m
  embed (AnnotF a t ty) = Annot a t ty
  embed (ErrorF a ty) = Error a ty


subterms :: Traversal' (Term tyname name a) (Term tyname name a)
subterms f = \case
  App a t u -> App a <$> f t <*> f u
  Let a n ty t -> Let a n ty <$> f t
  Annot a t ty -> (\t' -> Annot a t' ty) <$> f t
  t -> pure t
{-# INLINABLE subterms #-}

subtypes :: Traversal' (Term tyname name a) (Type tyname a)
subtypes f = \case
  Let a n ty t -> (\ty' -> Let a n ty' t) <$> f ty
  Annot a t ty -> Annot a t <$> f ty
  Error a ty -> Error a <$> f ty
  t -> pure t
{-# INLINABLE subtypes #-}

vars :: Traversal' (Term tyname name a) (name a)
vars = _Var . traverse
{-# INLINABLE vars #-}
