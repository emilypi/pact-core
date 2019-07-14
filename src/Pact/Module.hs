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
-- The pact modules
--
module Pact.Module
( RawModule(..)
, ResolvedModule(..)
, RawInterface(..)
, ResolvedInterface(..)
  -- * Lenses
, moduleName
, moduleDefns
, moduleExports
, moduleImports
, moduleConstraints
, mrawName
, mrawDefns
, mrawExports
, mrawImports
, mrawConstraints
, interfaceName
, interfaceDefns
, interfaceImports
, irawName
, irawDefns
, irawImports
) where


import GHC.Generics (Generic)

import Control.DeepSeq
import Control.Lens

import Data.Hashable
import Data.Text

import Pact.Declaration
import Pact.Expr
import Pact.Names
import Pact.Terms


data RawModule = Module
  { _mrawName :: !ModuleName
  , _mrawDefns :: [RawDecl]
  , _mrawExports :: [BasicName]
  , _mrawImports :: [ModuleName]
  , _mrawConstraints :: [ModuleName]
  } deriving (Eq, Show, Generic, NFData)
makeLenses ''RawModule

data ResolvedModule = ResolvedModule
  { _moduleName :: !ModuleName
  , _moduleDefns :: [ResolvedDecl]
  , _moduleExports :: [FullyQualified]
  , _moduleImports :: [ResolvedDecl]
  , _moduleConstraints :: [ModuleName]
  }
makeLenses ''ResolvedModule

data RawInterface = RawInterface
  { _irawName :: !ModuleName
  , _irawDefns :: [RawDecl]
  , _irawImports :: [ModuleName]
  } deriving (Eq, Show, Generic, NFData)
makeLenses ''RawInterface

data ResolvedInterface = ResolvedInterface
  { _interfaceName :: !ModuleName
  , _interfaceDefns :: [ResolvedDecl]
  , _interfaceImports :: [ModuleName]
  } deriving (Eq, Show, Generic, NFData)
makeLenses ''ResolvedInterface
