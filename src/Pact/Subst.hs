{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
-- |
-- Copyright :  (c) Emily Pillmore 2019-2019
-- License   :  BSD-2-Clause
-- Maintainer:  Emily Pillmore <emily@kadena.io>
-- Stability :  experimental
-- Portability: non-portable
--
-- Initial alpha-substitution state env and its transformer
--
module Pact.Subst where


import Control.Concurrent.Supply
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State

import Data.HashSet
import Data.IntMap.Lazy

import Pact.Aliases
import Pact.Env
import Pact.Names


-- | A substitution of unification variables for types or kinds
data SubstState = SubstState
  { _substType :: IntMap RawType
    -- ^ Type substitution
  , _substKind :: IntMap SourcedKind
    -- ^ Kind substitution
  , _substNames :: IntMap BasicName
    -- ^ substituted names
  , _substUsed :: HashSet Int
    -- ^ substituted names in use
  , _substSupply :: Supply
    -- ^ new name supply
  } deriving Show
makeLenses ''SubstState

newtype AlphaT a = AlphaT
  { _runAlphaT :: ReaderT Env (StateT SubstState IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState SubstState
             , MonadReader Env
             , MonadIO
             )
