{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module LowerAST(lowerMod) where

import qualified IR
import qualified AST

import Location

import qualified Data.Map as Map
import Data.List(foldl')

lowerMod :: AST.LDModule -> Maybe IR.Module
lowerMod lmod =
    case loweredMod of
        Just ir -> Just ir
        Nothing -> error "lowering ast to ir returned Nothing"
    where
        loweredMod = flip vdefine lmod . flip vdeclare lmod . flip ddefine lmod $ ddeclare Nothing lmod

class Lowerable l p where
    ddeclare, ddefine, vdeclare, vdefine :: p -> l -> p
class Parent p c where
    add :: p -> c -> p

type ModParent = Maybe IR.Module
instance Parent ModParent IR.Module where
    add _ m = Just m

instance Parent IR.DeclSymbol IR.Value where
    add parent child = undefined

instance (Parent p IR.Module) => Lowerable AST.LDModule p where
    ddeclare parent (Located _ (AST.DModule' decls)) =
        add parent finalModule
        where
            startModule = IR.DSModule $ IR.Module Map.empty Map.empty
            finalModule = foldl' ddeclare startModule decls
    ddefine = undefined
    vdeclare = undefined
    vdefine = undefined

instance Lowerable AST.LDDecl IR.DeclSymbol where
    -- ddeclare parentMod (Located _ AST.DModule'
