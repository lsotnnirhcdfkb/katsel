module LowerAST(lowerMod) where

import qualified IR
import qualified AST

import Location

lowerMod :: AST.LDModule -> Maybe IR.Unit
lowerMod (Located _ (AST.DModule' decls)) = error "TODO"

class Lowerable a where
    ddeclare, ddefine, vdeclare, vdefine :: IR.Unit -> a -> IR.Unit

