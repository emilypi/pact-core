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
-- Convenience synonyms
--
module Pact.Aliases
( -- * Synonyms
  RawType
, ResolvedType
, RawTerm
, ResolvedTerm
, SourcedKind
, SExpr
) where

import Pact.AST.SourcePos
import Pact.Declaration
import Pact.Expr
import Pact.Kinds
import Pact.Names
import Pact.Terms
import Pact.Types


-- | The type of types with source annotation and basic names
type RawType = Type BasicName SourceAnn
-- | The type of fully resolved type data
type ResolvedType = Type FullyQualified SourceAnn

-- | The type of raw terms
type RawTerm = Term BasicName SourceAnn
-- | The type of fully resolved terms
type ResolvedTerm = Term FullyQualified SourceAnn

-- | The type of kinds with annotation information
type SourcedKind = Kind SourceAnn

-- | The type of unresolved s-expressions
type SExpr = Expr BasicName SourceAnn
