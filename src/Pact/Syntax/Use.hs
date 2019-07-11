-- |
-- Sorting transitive module dependencies
-- via strongly-connect components
--
module Pact.Syntax.Use where


import Data.HashMap.Strict
import Data.Text

type ModuleName = Text
type Module = ()

type ModuleGraph = (ModuleName, HashMap ModuleName Module)


data Module
