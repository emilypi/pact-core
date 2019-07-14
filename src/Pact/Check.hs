module Pact.Check where


import Control.Concurrent.Supply
import Control.Lens

import Data.HashMap.Lazy

import Pact.AST.SourcePos
import Pact.Environment
import Pact.Kinds
import Pact.Names
import Pact.Types



-- | A substitution of unification variables for types or kinds
data Substitution = Substitution
  { substType :: HashMap Int (Type SourceAnn)
    -- ^ Type substitution
  , substKind :: HashMap Int (Kind SourceAnn)
    -- ^ Kind substitution
  }

-- | An empty substitution
emptySubstitution :: Substitution
emptySubstitution = Substitution mempty mempty


-- | State required for type checking
data CheckState = CheckState
  { _checkEnv :: Environment
    -- ^ The current @Environment@
  , _checkSupply :: Supply
    -- ^ the current supply state
  , _checkNextType :: {-# UNPACK #-} Int
    -- ^ The next type unification variable
  , _checkNextKind :: {-# UNPACK #-} Int
    -- ^ The next kind unification variable
  , _checkNextSkolem :: {-# UNPACK #-} Int
    -- ^ The next skolem variable
  , _checkNextSkolemScope :: {-# UNPACK #-} Int
    -- ^ The next skolem scope constant
  , _checkCurrentModule :: Maybe ModuleName
    -- ^ The current module
  , _checkSubstitution :: Substitution
    -- ^ The current substitution
  , _checkHints :: [ErrorMessageHint]
    -- ^ The current error message hint stack.
    -- This goes into state, rather than using 'rethrow',
    -- since this way, we can provide good error messages
    -- during instance resolution.
  }

-- | Create an empty @CheckState@
emptyCheckState :: Environment -> IO CheckState
emptyCheckState env = (\s -> CheckState env s 0 0 0 0 Nothing emptySubstitution []) <$> newSupply
