{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pact.Check
( -- * Data
  Substitution(..)
, CheckState(..)
  -- * Types
, TypeCheckM
  -- * Combinators
, emptySubstitution
, emptyCheckState
, runTypeCheckM
) where


import GHC.Generics

import Control.Concurrent.Supply
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

import Data.HashMap.Lazy

import Pact.AST.SourcePos
import Pact.Environment
import Pact.Hint
import Pact.Kinds
import Pact.Names
import Pact.Types



-- | The typechecking monad
--
newtype TypeCheckM a
    = TypeCheckM { _runTypeCheckM :: ReaderT Environment (StateT CheckState IO) a }
    deriving (Functor, Applicative, Monad, MonadState CheckState, MonadReader Environment, MonadIO)

runTypeCheckM :: Environment -> CheckState -> TypeCheckM a -> IO (a, CheckState)
runTypeCheckM e st (TypeCheckM m) = runStateT (runReaderT m e) st

-- | A substitution of unification variables for types or kinds
data Substitution = Substitution
  { substType :: HashMap Int (Type SourceAnn)
    -- ^ Type substitution
  , substKind :: HashMap Int (Kind SourceAnn)
    -- ^ Kind substitution
  } deriving Show

-- | An empty substitution
emptySubstitution :: Substitution
emptySubstitution = Substitution mempty mempty


-- | State required for type checking
data CheckState = CheckState
  { _checkEnv :: Environment
    -- ^ The current @Environment@
  , _checkSupply :: Supply
    -- ^ the current supply state
  , _checkNextType :: {-# UNPACK #-} !Int
    -- ^ The next type unification variable
  , _checkNextKind :: {-# UNPACK #-} !Int
    -- ^ The next kind unification variable
  , _checkNextSkolem :: {-# UNPACK #-} !Int
    -- ^ The next skolem variable1
  , _checkNextSkolemScope :: {-# UNPACK #-} !Int
    -- ^ The next skolem scope constant
  , _checkCurrentModule :: Maybe ModuleName
    -- ^ The current module
  , _checkSubstitution :: Substitution
    -- ^ The current substitution
  , _checkHints :: [ErrorHint]
    -- ^ The current error message hint stack.
    -- This goes into state, rather than using 'rethrow',
    -- since this way, we can provide good error messages
    -- during instance resolution.
  } deriving (Show)


-- | Create an empty @CheckState@
emptyCheckState :: Environment -> IO CheckState
emptyCheckState env = (\s -> CheckState env s 0 0 0 0 Nothing emptySubstitution []) <$> newSupply
