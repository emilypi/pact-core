{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}
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
-- Names of various shapes and sizes
--
module Pact.Names
( -- * Data
  Ident(..)
, NameSort(..)
, Namespace(..)
, NameVisibility(..)
, FullyQualified(..)
, NameOrigin(..)
, CoreName(..)
, ModuleName(..)
  -- * Optics
, fqName
, fqModule
, fqNamesort
, fqUnique
, fqNameVisibility
, coreName
, coreModule
, coreOrigin
, coreNameSort
  -- * Combinators
, ppIdent
) where


import GHC.Generics

import Control.DeepSeq
import Control.Lens

import Data.Hashable
import Data.Text
import Data.Word


import Pact.Utils

data Ident
  = Ident {-# UNPACK #-} !Text
    -- ^ Alphanumeric identifiers
  | GenIdent !(Maybe Text) {-# UNPACK #-} !Int
    -- ^ Generated names/de bruijn indices
  | UnusedIdent
  deriving (Show, Eq, Ord, Hashable, Generic)

instance NFData Ident

makePrisms ''Ident

ppIdent :: Ident -> Text
ppIdent (Ident t) = t
ppIdent (GenIdent Nothing t) = "$" <> tshow t
ppIdent (GenIdent (Just n) t) = "$" <> n <> tshow t
ppIdent UnusedIdent = "$~unused"

data NameOrigin
  = LocalOrigin
  | TopLevelOrigin
  | BuiltInOrigin
  | ImportedOrigin
  deriving (Eq, Ord, Show, Generic, Hashable, NFData)

data NameSort
  = Private
  | Public
  | Repl
  deriving (Eq, Ord, Show, Generic, Hashable, NFData)

data NameVisibility
  = Undefined
  | Defined
  deriving (Eq, Ord, Show, Generic, Hashable, NFData)

data Namespace
  = Local {-# UNPACK #-} !Text
  | Global
  deriving (Eq, Ord, Show, Generic, NFData, Hashable)

data ModuleName = ModuleName
  { _moduleNamespace :: {-# UNPACK #-} !Namespace
  , _moduleName :: {-# UNPACK #-} !Text
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)
makeLenses ''ModuleName

data CoreName = CoreName
  { _coreName :: {-# UNPACK #-} !Text
  , _coreOrigin :: {-# UNPACK #-} !NameOrigin
  , _coreModule :: !ModuleName
  , _coreNameSort :: !NameSort
  } deriving (Eq, Ord, Show, Hashable, Generic, NFData)
makeLenses ''CoreName

data FullyQualified = FullyQualified
  { _fqName :: {-# UNPACK #-} !Text
  , _fqUnique :: {-# UNPACK #-} !Word64
  , _fqModule :: {-# UNPACK #-} !ModuleName
  , _fqNamesort :: !NameSort
  , _fqNameVisibility :: !NameVisibility
  } deriving (Eq, Ord, Show, Hashable, Generic, NFData)
makeLenses ''FullyQualified
