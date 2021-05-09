{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Module
    ( Module
    , new_module
    ) where

import IR.Parent
import IR.MapSynonyms
import IR.IRCtx
import IR.DeclSymbol
import IR.Type
import IR.Value
import IR.DeclSpan
import IR.Describe
import IR.TypeInterner

import qualified Data.Map as Map

import Location

data Module = Module DSMap VMap Span
new_module :: Span -> IRCtx -> (Module, IRCtx)
new_module sp irctx = (Module Map.empty Map.empty sp, irctx)
    where
        {-
        make_list name ty (tys, ctx) =
            let (idx, ctx') = get_ty ty ctx
            in (tys ++ [(name, DeclSymbol idx)], ctx')

        dsmap = Map.fromList dsmap_list
        (dsmap_list, tyctx') =
            make_list "void" (VoidType Map.empty) .
            make_list "float" (FloatType Map.empty 32) .
            make_list "double" (FloatType Map.empty 64) .
            make_list "bool" (BoolType Map.empty) .
            make_list "char" (CharType Map.empty) .
            make_list "uint8" (IntType Map.empty 8 Unsigned) .
            make_list "uint16" (IntType Map.empty 16 Unsigned) .
            make_list "uint32" (IntType Map.empty 32 Unsigned) .
            make_list "uint64" (IntType Map.empty 64 Unsigned) .
            make_list "sint8" (IntType Map.empty 8 Signed) .
            make_list "sint16" (IntType Map.empty 16 Signed) .
            make_list "sint32" (IntType Map.empty 32 Signed) .
            make_list "sint64" (IntType Map.empty 64 Signed) $ ([], tyctx)
        -}

instance ParentR Module DeclSymbol String where
    get_child_map (Module dsmap _ _) = dsmap
instance ParentR Module Value String where
    get_child_map (Module _ vmap _) = vmap

instance ParentW Module DeclSymbol String where
    add name ds (Module dsmap vmap sp) =
        let old_val = Map.lookup name dsmap
            dsmap' = Map.insert name ds dsmap
        in (old_val, Module dsmap' vmap sp)
instance ParentW Module Value String where
    add name ds (Module dsmap vmap sp) =
        let old_val = Map.lookup name vmap
            vmap' = Map.insert name ds vmap
        in (old_val, Module dsmap vmap' sp)

instance DeclSpan Module where
    decl_span (Module _ _ sp) = Just sp
instance Describe Module where
    describe _ = "module being compiled"
