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
, BasicName(..)
, ModuleName(..)
, Saturation(..)
  -- * Optics
, fqName
, fqModule
, fqNamesort
, fqUnique
, fqNameVisibility
, fqArity
, fqSaturation
, basicName
, basicModule
, basicOrigin
  -- * Combinators
, ppIdent
, coreName
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

data Saturation = Saturated | Unsaturated
  deriving (Eq, Ord, Show, Hashable, Generic, NFData)

data ModuleName = ModuleName
  { _moduleNamespace :: !Namespace
  , _moduleName :: !(Maybe Text)
  } deriving (Eq, Ord, Show, Generic, NFData, Hashable)
makeLenses ''ModuleName


data BasicName = BasicName
  { _basicName :: {-# UNPACK #-} !Text
  , _basicOrigin :: !NameOrigin
  , _basicModule :: !ModuleName
  } deriving (Eq, Ord, Show, Hashable, Generic, NFData)
makeLenses ''BasicName


data FullyQualified = FullyQualified
  { _fqName :: {-# UNPACK #-} !Text
  , _fqUnique :: {-# UNPACK #-} !Word64
  , _fqModule :: !ModuleName
  , _fqNamesort :: !NameSort
  , _fqArity :: {-# UNPACK #-} !Word64
  , _fqSaturation :: Saturation
  , _fqNameVisibility :: !NameVisibility
  } deriving (Eq, Ord, Show, Hashable, Generic, NFData)
makeLenses ''FullyQualified

coreName :: Text -> BasicName
coreName n = BasicName n BuiltInOrigin (ModuleName Global Nothing)
