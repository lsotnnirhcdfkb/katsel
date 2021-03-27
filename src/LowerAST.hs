module LowerAST(lowerCU) where

import qualified IR
import qualified AST

import Location

data CGD0
    = FunctionCGD0 AST.LSFunDecl
    | ImplCGD0 AST.LDType [CGD0]

data CGD1
    = FunctionCGD1
    | ImplCGD1

data CGV0
    = FunctionCGV0
    | ImplCGV0

data CGV1
    = FunctionCGV1
    | ImplCGV1

lowerCU :: AST.LDCU -> Maybe IR.Unit
lowerCU (Located _ (AST.DCU'CU decls)) = error "TODO"
    where
        declcgs = map declCG decls

declCG :: AST.LDDecl -> CGD0
declCG (Located _ (AST.DDecl'Fun sf)) = FunctionCGD0 sf
declCG (Located _ (AST.DDecl'Impl implFor members)) = ImplCGD0 implFor $ map implMemberCG members

implMemberCG :: AST.LDImplMember -> CGD0
implMemberCG (Located _ (AST.DImplMember'Fun sf)) = FunctionCGD0 sf
