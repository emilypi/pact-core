{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module Pact.Syntax.Term where

import GHC.Generics

import Control.DeepSeq
import Control.Lens

import Data.Functor.Foldable
import Data.Map
import Data.Text

import Pact.Syntax.Type


data Builtin a = BuiltinName a String
  deriving (Eq, Show, Functor, Generic, NFData)

type Decimal = Double
type Time = Double

data Constant a
  = BuiltInInteger a Integer
  | BuiltInDecimal a Decimal
  | BuiltInString a Text
  | BuiltInTime a Time
  deriving (Eq, Show, Functor, Generic, NFData)

data Term tyname name loc
  = Var loc (name loc)
    -- ^ named variable
  | Let loc (name loc) (Type tyname loc) (Term tyname name loc)
    -- ^ let bindings. Note: 'let x:y = m in n' desugars to
    -- (\x:y -> n) m, hence we just make use of lam and app
  | App loc (Term tyname name loc) (Term tyname name loc)
    -- ^ β-reduction
  | Constant loc (Constant loc)
    -- ^ constant terms
  | Builtin loc (Builtin loc)
    -- ^ builtin terms
  | Annot loc (Term tyname name loc) (Type tyname loc)
    -- ^ type annotation
  | Binding loc (Type tyname loc) (Map String (Term tyname name loc))
    -- ^
  | Error loc (Type tyname loc)
    -- ^
  deriving (Show, Functor, Generic, NFData)

makePrisms ''Term

data TermF tyname name a x
  = VarF a (name a)
  | LetF a (name a) (Type tyname a) x
  | AppF a x x
  | BindingF a (Type tyname a) (Map String x)
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
  project (Binding a ty m) = BindingF a ty m
  project (Annot a t ty) = AnnotF a t ty
  project (Error a ty) = ErrorF a ty

instance Corecursive (Term tyname name a) where
  embed (VarF a n) = Var a n
  embed (LetF a n ty t) = Let a n ty t
  embed (AppF a t u) = App a t u
  embed (ConstantF a c) = Constant a c
  embed (BuiltinF a b) = Builtin a b
  embed (BindingF a ty m) = Binding a ty m
  embed (AnnotF a t ty) = Annot a t ty
  embed (ErrorF a ty) = Error a ty


subterms :: Traversal' (Term tyname name a) (Term tyname name a)
subterms f = \case
  App a t u -> App a <$> f t <*> f u
  Let a n ty t -> Let a n ty <$> f t
  Annot a t ty -> (\t' -> Annot a t' ty) <$> f t
  t -> pure t

subtypes :: Traversal' (Term tyname name a) (Type tyname a)
subtypes f = \case
  Let a n ty t -> (\ty' -> Let a n ty' t) <$> f ty
  Annot a t ty -> Annot a t <$> f ty
  Error a ty -> Error a <$> f ty
  t -> pure t

vars :: Traversal' (Term tyname name a) (name a)
vars = _Var . traverse
