module LowerAST(lowerMod) where

import qualified IR
import qualified AST

import Location

data CGD0
    = ModuleCGD0 [CGD0]
    | FunctionCGD0 CGD0 AST.LSFunDecl
    | ImplCGD0 AST.LDType [CGD0]

data CGD1
    = ModuleCGD1
    | FunctionCGD1
    | ImplCGD1

data CGV0
    = ModuleCGV0
    | FunctionCGV0
    | ImplCGV0

data CGV1
    = ModuleCGV1
    | FunctionCGV1
    | ImplCGV1

lowerMod :: AST.LDModule -> Maybe IR.Unit
lowerMod (Located _ (AST.DModule' decls)) = error "TODO"
    where
        modcg = ModuleCGD0 $ map (declCG modcg) decls

declCG :: CGD0 -> AST.LDDecl -> CGD0
declCG parent (Located _ (AST.DDecl'Fun sf)) = FunctionCGD0 parent sf
declCG _ (Located _ (AST.DDecl'Impl implFor members)) = parentCG
    where
        parentCG = ImplCGD0 implFor $ map (implMemberCG parentCG) members

implMemberCG :: CGD0 -> AST.LDImplMember -> CGD0
implMemberCG parent (Located _ (AST.DImplMember'Fun sf)) = FunctionCGD0 parent sf
