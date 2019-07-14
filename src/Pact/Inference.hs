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
module Pact.Inference where

import Control.Concurrent.Supply
import Control.Monad.State

import Data.HashMap.Strict

import Data.Text
import Data.Word

import Pact.Aliases
import Pact.AST.SourcePos
import Pact.Infer
import Pact.Declaration
import Pact.Kinds
import Pact.Module
import Pact.Terms
import Pact.Types


inferDecl :: RawDecl -> InferM ()
inferDecl (ConstantDecl n l) = undefined
