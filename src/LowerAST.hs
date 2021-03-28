module LowerAST(lowerMod) where

import qualified IR
import qualified AST

import Location

lowerMod :: AST.LDModule -> Maybe IR.Unit
lowerMod (Located _ (AST.DModule' decls)) = error "TODO"

ddeclare, ddefine, vdeclare, vdefine :: IR.Unit -> AST.DDecl -> IR.Unit

ddeclare = undefined
ddefine = undefined
vdeclare = undefined
vdefine = undefined
