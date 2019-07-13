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
( Module(..)
  -- * Lenses
, moduleName
, moduleDefns
, moduleExports
, moduleImports
, moduleTerms
, interfaceName
, interfaceDefns
, interfaceTerms
, interfaceImports
) where


import GHC.Generics (Generic)

import Control.DeepSeq
import Control.Lens

import Data.Hashable
import Data.Text

import Pact.Declaration
import Pact.Names
import Pact.Terms

data Module a = Module
  { _moduleName :: !ModuleName
  , _moduleDefns :: [Declaration a]
  , _moduleExports :: [Text]
  , _moduleImports :: [ModuleName]
  , _moduleConstraints :: [ModuleName]
  , _moduleTerms :: [Term a]
  } deriving (Eq, Show, Functor, Generic, NFData)

makeLenses ''Module


data Interface a = Interface
  { _interfaceName :: !ModuleName
  , _interfaceDefns :: [Declaration a]
  , _interfaceImports :: [ModuleName]
  , _interfaceTerms :: [Term a]
  } deriving (Eq, Show, Functor, Generic, NFData)

makeLenses ''Interface
