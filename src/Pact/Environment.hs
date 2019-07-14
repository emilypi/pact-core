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
-- Type analysis environment with primitive types and names
--
module Pact.Environment where


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


data Environment = Environment
  { _envNames :: !(HashMap BasicName RawType)
  , _envTypes :: !(HashMap BasicName (SourcedKind, TypeSort))
  } deriving (Show, Generic, NFData)

envNames :: Lens' Environment (HashMap BasicName RawType)
envNames = lens _envNames (\t b -> t { _envNames = b })

envTypes :: Lens' Environment (HashMap BasicName (SourcedKind, TypeSort))
envTypes = lens _envTypes (\t b -> t { _envTypes = b })

instance Semigroup Environment where
  Environment n t <> Environment n' t' = Environment (n <> n') (t <> t')

instance Monoid Environment where
  mempty = Environment HashMap.empty HashMap.empty

initEnvironment :: Environment
initEnvironment = mempty
  & envNames .~ primNames
  & envTypes .~ primTypes

primKind :: Kind SourceAnn
primKind = KType initSourceAnn

primType :: Prim -> RawType
primType = TyBuiltin initSourceAnn

primTypes :: HashMap BasicName (SourcedKind, TypeSort)
primTypes = HashMap.fromList
  [ (integer, (primKind, Primitive))
  , (boolean, (primKind, Primitive))
  , (time, (primKind, Primitive))
  , (string, (primKind, Primitive))
  , (decimal, (primKind, Primitive))
  , (guard, (primKind, Security))
  ]

primNames :: HashMap BasicName RawType
primNames = HashMap.fromList
  [ (true , primType TyBool)
  , (false, primType TyBool)
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
