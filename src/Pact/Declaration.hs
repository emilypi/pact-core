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
) where


import GHC.Generics

import Control.DeepSeq
import Control.Lens
import Data.Hashable
import Data.Text

import Pact.AST.Literals
import Pact.Names
import Pact.Terms
import Pact.Types


data Declaration a
  = TermDecl {-# UNPACK #-} !Text (Term a) (Type a)
  | ConstantDecl {-# UNPACK #-} !Text !(Literal a)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic, NFData)
makePrisms ''Declaration


terms :: Traversal' (Declaration a) (Term a)
terms f = \case
  TermDecl n t ty -> (\t' -> TermDecl n t' ty) <$> f t
  t -> pure t

types :: Traversal' (Declaration a) (Type a)
types f = \case
  TermDecl n t ty -> TermDecl n t <$> f ty
  t -> pure t
