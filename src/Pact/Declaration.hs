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
module Pact.Declaration
( Declaration(..)
  -- * Prisms
, _TermDecl
, _ConstantDecl
  -- * Synonyms
, RawDecl
, ResolvedDecl
) where


import GHC.Generics

import Control.DeepSeq
import Control.Lens
import Data.Hashable
import Data.Text

import Pact.AST.SourcePos
import Pact.AST.Literals
import Pact.Names
import Pact.Terms
import Pact.Types

-- | Unresolved Decls
type RawDecl = Declaration BasicName SourceAnn

-- | Resoloved decls with qualified names
type ResolvedDecl = Declaration FullyQualified SourceAnn

data Declaration n a
  = TermDecl n (Term n a)
  | ConstantDecl n !(Literal a)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic, NFData)
makePrisms ''Declaration


terms :: Traversal' (Declaration n a) (Term n a)
terms f = \case
  TermDecl n t -> TermDecl n <$> f t
  t -> pure t
