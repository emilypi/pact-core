{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Pact.Infer
( -- * Data
  Substitution(..)
, InferState(..)
  -- * Types
, InferM
  -- * Combinators
, emptySubstitution
, emptyInferState
, runInferM
  -- * Optics
, substType
, substKind
, inferEnv
, inferSupply
, inferNextType
, inferNextKind
, inferNextSkolem
, inferNextSkolemScope
, inferSubstitution
, inferHints
) where


import GHC.Generics

import Control.Concurrent.Supply
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

import Data.HashMap.Lazy

import Pact.Aliases
import Pact.AST.SourcePos
import Pact.Environment
import Pact.Hint
import Pact.Kinds
import Pact.Names
import Pact.Types



-- | A substitution of unification variables for types or kinds
data Substitution = Substitution
  { _substType :: HashMap Int RawType
    -- ^ Type substitution
  , _substKind :: HashMap Int SourcedKind
    -- ^ Kind substitution
  } deriving Show
makeLenses ''Substitution

-- | An empty @substitution@
emptySubstitution :: Substitution
emptySubstitution = Substitution mempty mempty

-- | State required for type infering
data InferState = InferState
  { _inferEnv :: Environment
    -- ^ The current @Environment@
  , _inferNextType :: {-# UNPACK #-} !Int
    -- ^ The next type unification variable
  , _inferNextKind :: {-# UNPACK #-} !Int
    -- ^ The next kind unification variable
  , _inferNextSkolem :: {-# UNPACK #-} !Int
    -- ^ The next skolem variable1
  , _inferNextSkolemScope :: {-# UNPACK #-} !Int
    -- ^ The next skolem scope constant
  , _inferCurrentModule :: Maybe ModuleName
    -- ^ The current module
  , _inferSubstitution :: Substitution
    -- ^ The current substitution
  , _inferHints :: [ErrorHint]
    -- ^ The current error message hint stack.
    -- This goes into state, rather than using 'rethrow',
    -- since this way, we can provide good error messages
    -- during instance resolution.
  , _inferSupply :: Supply
    -- ^ the current supply state
  } deriving Show
makeLenses ''InferState

-- | Create an empty @InferState@
emptyInferState :: IO InferState
emptyInferState = InferState initEnvironment 0 0 0 0 Nothing emptySubstitution [] <$> newSupply

-- | The type inference monad
--
newtype InferM a
    = InferM { _runInferM :: ReaderT Environment (StateT InferState IO) a }
    deriving (Functor, Applicative, Monad, MonadState InferState, MonadReader Environment, MonadIO)

runInferM :: Environment -> InferState -> InferM a -> IO (a, InferState)
runInferM e st (InferM m) = runStateT (runReaderT m e) st
