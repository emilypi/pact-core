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
-- Pact declarations
--
module Pact.Typechecker where

import Control.Concurrent.Supply
import Control.Monad.State

import Data.HashMap.Strict
import Data.Text
import Data.Word


data Env = Env
  { _envSupply :: Supply
  , _envAllRenamed :: HashMap Text (HashMap Text Word64)
  }
-- type TypeCheckM ann = ReaderT TypeCheckEnv (ExceptT (TypeError ann) Quote)
