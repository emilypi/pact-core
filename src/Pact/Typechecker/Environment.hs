module Pact.Typechecker.Environment where

-- | A substitution of unification variables for types or kinds
data Substitution = Substitution
  { substType :: M.Map Int SourceType -- ^ Type substitution
  , substKind :: M.Map Int SourceKind -- ^ Kind substitution
  }

-- | An empty substitution
emptySubstitution :: Substitution
emptySubstitution = Substitution M.empty M.empty

-- | State required for type checking
data CheckState = CheckState
  { checkEnv :: Environment
  -- ^ The current @Environment@
  , checkNextType :: Int
  -- ^ The next type unification variable
  , checkNextKind :: Int
  -- ^ The next kind unification variable
  , checkNextSkolem :: Int
  -- ^ The next skolem variable
  , checkNextSkolemScope :: Int
  -- ^ The next skolem scope constant
  , checkCurrentModule :: Maybe ModuleName
  -- ^ The current module
  , checkSubstitution :: Substitution
  -- ^ The current substitution
  , checkHints :: [ErrorMessageHint]
  -- ^ The current error message hint stack.
  -- This goes into state, rather than using 'rethrow',
  -- since this way, we can provide good error messages
  -- during instance resolution.
  }
