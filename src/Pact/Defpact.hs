{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- Copyright :  (c) Emily Pillmore 2019-2019
-- License   :  BSD-2-Clause
-- Maintainer:  Emily Pillmore <emily@kadena.io>
-- Stability :  experimental
-- Portability: non-portable
--
-- Pact defpact syntax
--
module Pact.Defpact
( Yield(..)
, Resume(..)
, Step(..)
, Defpact(..)
, PactSort(..)
, stepName
, stepNumber
, stepRollback
, stepYield
, stepResume
, stepReturnType
, stepDefn
, pactArgs
, pactSteps
, pactName
, pactSort
)where


import GHC.Generics

import Control.DeepSeq
import Control.Lens

import Data.HashMap.Strict
import Data.List.NonEmpty
import Data.Text
import Data.Word

import Pact.Names
import Pact.Types
import Pact.Terms


data Yield a = Yield a (Term a) (Type a)
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic, NFData)

data Resume a = Resume a (Term a) (Type a)
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic, NFData)

data Step a = Step
  { _stepName :: Maybe Text
    -- ^ If labeled, the step name (note: private pacts only)
  , _stepNumber :: {-# UNPACK #-} !Word64
    -- ^ step number: uints starting at 0
  , _stepRollback :: Maybe (Term a)
    -- ^ if `step-with-rollback`, then optionally specify a rollback
  , _stepYield :: Maybe (Yield a)
    -- ^ yield data, if present
  , _stepResume :: Maybe (Resume a)
    -- ^ resume data, if present (must resume on previous step)
  , _stepReturnType :: Type a
    -- ^ return type spec for return from pact step
  , _stepDefn :: Term a
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic, NFData)
makeLenses ''Step

data PactSort = Public | Private
  deriving (Show, Eq, Generic, NFData)

data Defpact a = Defpact
  { _pactArgs :: HashMap Text (Type a)
  , _pactSteps :: NonEmpty (Step a)
  , _pactName :: !FullyQualified
  , _pactSort :: PactSort
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic, NFData)
makeLenses ''Defpact
