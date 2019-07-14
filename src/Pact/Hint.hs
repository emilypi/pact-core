module Pact.Hint
( ErrorHint(..)
) where


import Data.List.NonEmpty
import Data.Text

import Pact.Aliases
import Pact.AST.SourcePos
import Pact.Expr
import Pact.Names
import Pact.Types

data ErrorHint
  = ErrorUnifyingTypes RawType RawType
  | ErrorInExpression SExpr
  | ErrorInModule ModuleName
  | ErrorInSubsumption RawType RawType
  | ErrorCheckingAccessor SExpr {-# UNPACK #-} !Text
  | ErrorCheckingType SExpr RawType
  | ErrorCheckingKind RawType
  | ErrorInferringType SExpr
  | ErrorInApplication SExpr RawType SExpr
  | ErrorInBindingGroup (NonEmpty Ident)
  | ErrorInValueDeclaration Ident
  | ErrorInTypeDeclaration Ident
  | PositionedError (NonEmpty SourceSpan)
  deriving (Show)
