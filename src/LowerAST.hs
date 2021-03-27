module LowerAST(lowerCU) where

import qualified IR
import qualified AST

import Location

data CGT0
    = FunctionCGT0 AST.LSFunDecl
    | ImplCGT0 AST.LDType [CGT0]

data CGT1
    = FunctionCGT1
    | ImplCGT1

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

declCG :: AST.LDDecl -> CGT0
declCG (Located _ (AST.DDecl'Fun sf)) = FunctionCGT0 sf
declCG (Located _ (AST.DDecl'Impl implFor members)) = ImplCGT0 implFor $ map implMemberCG members

implMemberCG :: AST.LDImplMember -> CGT0
implMemberCG (Located _ (AST.DImplMember'Fun sf)) = FunctionCGT0 sf
