{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- Copyright :  (c) Emily Pillmore 2019-2019
-- License   :  BSD-2-Clause
-- Maintainer:  Emily Pillmore <emily@kadena.io>
-- Stability :  experimental
-- Portability: non-portable
--
-- Pact declarations
--
module Pact.Inference where

import Control.Concurrent.Supply
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import Data.HashMap.Strict
import Data.List.NonEmpty
import Data.Text
import Data.Word

import Pact.Aliases
import Pact.AST.SourcePos
import Pact.Declaration
import Pact.Environment
import Pact.Hint
import Pact.Kinds
import Pact.Module
import Pact.Subst
import Pact.Terms
import Pact.Types


-- | The type inference monad
--
newtype InferM a = InferM
  { _runInferM :: ReaderT Environment (StateT Substitution (ExceptT [ErrorHint] IO)) a
  }
  deriving (Functor, Applicative, Monad, MonadReader Environment, MonadIO)

runInferM :: Environment -> Substitution -> InferM a -> IO (Either [ErrorHint] (a, Substitution))
runInferM e s (InferM m) = runExceptT (runStateT (runReaderT m e) s)


lookupTyVar = undefined

-- | Infer the kind of a given type
--
-- note: v ~ infer, ^ ~ check
inferKind :: RawType -> InferM (Kind ())

-- a : k
-- ------------------
-- Γ :- prim a : k
--
inferKind (TyBuiltin _ _) =
  return $ KType ()

-- Γ :- v : k
-- ------------------
-- Γ :- var v : k
--
inferKind (TyVar _ v) =
  lookupTyVar v

-- Γ, l : a :- m : b
-- ------------------
-- Γ :- (\(l : a) -> m) : a -> b
--
inferKind (TyFun _ n (a:|[]) b) = undefined

-- Inductively,
--
-- Γ, (l, a)_{i ∊ N, i < 0} :- m : b
-- ------------------
-- Γ :- (\((l_1 : a_1) ... (l_n : a_n)) -> m) :- (a_1,...,a_n) -> b
--
