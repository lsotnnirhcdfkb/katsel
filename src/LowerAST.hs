{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

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
class Parent p c i | p c -> i where
    add :: p -> i -> c -> p
    get :: p -> i -> Maybe c

type ModParent = Maybe IR.Module
instance Parent ModParent IR.Module () where
    add _ _ m = Just m
    get = undefined

instance Parent IR.DeclSymbol IR.DeclSymbol String where
    add = undefined
    get = undefined
instance Parent IR.DeclSymbol IR.Value String where
    add = undefined
    get = undefined
instance Parent IR.Module IR.Value String where
    add = undefined
    get = undefined
instance Parent IR.Module IR.DeclSymbol String where
    add = undefined
    get = undefined

instance Parent p IR.Module () => Lowerable AST.LDModule p where
    ddeclare parent (Located _ (AST.DModule' decls)) = add parent () finalModule
        where
            startModule = IR.Module Map.empty Map.empty
            finalModule = foldl' ddeclare startModule decls

    ddefine parent (Located _ (AST.DModule' decls)) = add parent () defined
        where
            (Just parentmod) = get parent () :: Maybe IR.Module -- not sure why this type annotation is needed to compile
            defined = foldl' ddefine parentmod decls

    vdeclare = undefined
    vdefine = undefined

instance Parent p IR.Value String => Lowerable AST.LDDecl p where
    ddeclare _ (Located _ (AST.DDecl'Fun _)) = undefined
    ddeclare _ (Located _ (AST.DDecl'Impl _ _)) = undefined

    ddefine = undefined
    vdeclare = undefined
    vdefine = undefined
