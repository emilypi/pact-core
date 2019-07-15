{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright :  (c) Emily Pillmore 2019-2019
-- License   :  BSD-2-Clause
-- Maintainer:  Emily Pillmore <emily@kadena.io>
-- Stability :  experimental
-- Portability: non-portable
--
-- Initial name envionment with primitives
--
module Pact.Env
( -- * Data
  Env(..)
  -- * Optics
, envTypes
, envKinds
  -- * Defaults
, initEnv
) where


import GHC.Generics

import Control.DeepSeq
import Control.Lens

import Data.HashMap.Strict as HashMap
import Data.Text

import Pact.Aliases
import Pact.AST.SourcePos
import Pact.Kinds
import Pact.Names
import Pact.Types


data Env = Env
  { _envTypes :: HashMap BasicName RawType
    -- ^ value names pointing at their type
  , _envKinds :: HashMap BasicName SourcedKind
    -- ^ type names associated with their kind
  } deriving Show

envTypes :: Lens' Env (HashMap BasicName RawType)
envTypes = lens _envTypes (\t b -> t { _envTypes = b })

envKinds :: Lens' Env (HashMap BasicName SourcedKind)
envKinds = lens _envKinds (\t b -> t { _envKinds = b })

initEnv :: Env
initEnv = Env primTypes primKinds

primKind :: SourcedKind
primKind = KType initSourceAnn

primType :: Prim -> RawType
primType = TyBuiltin initSourceAnn

unitType :: RawType
unitType = TyUnit initSourceAnn

primKinds :: HashMap BasicName SourcedKind
primKinds = HashMap.fromList
  [ (integer, primKind)
  , (boolean, primKind)
  , (time, primKind)
  , (string, primKind)
  , (decimal, primKind)
  , (guard, primKind)
  , (unit, primKind)
  ]

primTypes :: HashMap BasicName RawType
primTypes = HashMap.fromList
  [ (true, primType TyBool)
  , (false, primType TyBool)
  , (unit, unitType)
  ]

integer, boolean, time, string, decimal, guard :: BasicName
integer = coreName "integer"
boolean = coreName "boolean"
time = coreName "time"
decimal = coreName "decimal"
guard = coreName "guard"
string = coreName "string"
true = coreName "true"
false = coreName "false"
unit = coreName "unit"
