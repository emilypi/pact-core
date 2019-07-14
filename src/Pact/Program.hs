module Pact.Program where

import Pact.Aliases
import Pact.Module
import Pact.Names
import Pact.Terms

-- | Toplevel program declarations
data RawTopLevel
  = RawModuleDecl RawModule
  | RawKeysetDecl RawTerm
  | RawTableDecl RawTerm
  | RawImportDecl ModuleName

data ResolvedTopLevel
  = ModuleDecl ResolvedModule
  | KeysetDecl ResolvedTerm
  | TableDecl ResolvedTerm
  | ResolvedImport ModuleName


type PreProgram = [RawTopLevel]
type Program = [ResolvedTopLevel]

runProgram :: PreProgram -> Program
runProgram = undefined

