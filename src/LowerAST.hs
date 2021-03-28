module LowerAST(lowerMod) where

import qualified IR
import qualified AST

import Location

-- each cg represents the data needed in order to do the phase
-- so DDeclCG represents the data needed to do the declaration declaration phase
-- and VDefCG represents the data needed to do the value definition phase

data DDeclCG
    = ModuleDDeclCG [DDeclCG]
    | FunctionDDeclCG DDeclCG AST.LSFunDecl
    | ImplDDeclCG AST.LDType [DDeclCG]

data DDefCG
    = ModuleDDefCG
    | FunctionDDefCG
    | ImplDDefCG

data VDeclCG
    = ModuleVDeclCG
    | FunctionVDeclCG
    | ImplVDeclCG

data VDefCG
    = ModuleVDefCG
    | FunctionVDefCG
    | ImplVDefCG

lowerMod :: AST.LDModule -> Maybe IR.Unit
lowerMod (Located _ (AST.DModule' decls)) = error "TODO"
    where
        modcg = ModuleDDeclCG $ map (declCG modcg) decls

declCG :: DDeclCG -> AST.LDDecl -> DDeclCG
declCG parent (Located _ (AST.DDecl'Fun sf)) = FunctionDDeclCG parent sf
declCG _ (Located _ (AST.DDecl'Impl implFor members)) = parentCG
    where
        parentCG = ImplDDeclCG implFor $ map (implMemberCG parentCG) members

implMemberCG :: DDeclCG -> AST.LDImplMember -> DDeclCG
implMemberCG parent (Located _ (AST.DImplMember'Fun sf)) = FunctionDDeclCG parent sf

ddecl :: DDeclCG -> DDefCG
ddecl = undefined

ddef :: DDefCG -> VDeclCG
ddef = undefined

vdecl :: VDeclCG -> VDefCG
vdecl = undefined

vdef :: VDefCG -> ()
vdef = undefined
