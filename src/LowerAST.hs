{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module LowerAST(lowerMod) where

import qualified IR
import qualified AST

import Location

import qualified Data.Map as Map
import Data.List(foldl')

lowerMod :: AST.LDModule -> IR.Module
lowerMod lmod =
    case loweredMod of
        Just ir -> ir
        Nothing -> error "lowering ast to ir returned Nothing"
    where
        ddeclared = ddeclare Nothing lmod
        ddefined = ddefine ddeclared lmod
        vdeclared = vdeclare ddefined lmod
        vdefined = vdefine vdeclared lmod
        loweredMod = vdefined

class Lowerable l p where
    ddeclare, ddefine, vdeclare, vdefine :: p -> l -> p
class Parent p c where
    add :: p -> c -> p

type ModParent = Maybe IR.Module
instance Parent ModParent IR.Module where
    add _ m = Just m

instance Parent IR.DeclSymbol IR.DeclSymbol where
    add = undefined
instance Parent IR.DeclSymbol IR.Value where
    add = undefined
instance Parent IR.Module IR.Value where
    add = undefined
instance Parent IR.Module IR.DeclSymbol where
    add = undefined

instance Parent p IR.Module => Lowerable AST.LDModule p where
    ddeclare parent (Located _ (AST.DModule' decls)) = add parent finalModule
        where
            startModule = IR.Module Map.empty Map.empty
            finalModule = foldl' ddeclare startModule decls

    ddefine = undefined
    vdeclare = undefined
    vdefine = undefined

instance Parent p IR.Value => Lowerable AST.LDDecl p where
    ddeclare _ (Located _ (AST.DDecl'Fun _)) = undefined
    ddeclare _ (Located _ (AST.DDecl'Impl _ _)) = undefined

    ddefine = undefined
    vdeclare = undefined
    vdefine = undefined
