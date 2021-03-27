module LowerAST(lowerCU) where

import qualified IR
import qualified AST

import Location

data CGT0
    = FunctionCGT0
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
declCG (Located _ (AST.DDecl'Fun sf)) = funCG sf
declCG (Located _ (AST.DDecl'Impl implFor members)) = ImplCGT0 implFor $ map implMemberCG members

funCG :: AST.LSFunDecl -> CGT0
funCG (Located _ (AST.SFunDecl' _ _ _ _)) = FunctionCGT0

implMemberCG :: AST.LDImplMember -> CGT0
implMemberCG (Located _ (AST.DImplMember'Fun sf)) = funCG sf
