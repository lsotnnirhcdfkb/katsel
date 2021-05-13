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

import qualified Data.Map as Map

import Location

data Module = Module DSMap VMap Span
new_module :: Span -> IRCtx -> (Module, IRCtx)
new_module sp irctx = (Module dsmap Map.empty sp, irctx')
    where
        make_list name ty (tys, ctx) =
            let (idx, ctx') = get_ty_irctx ty ctx
            in (tys ++ [(name, DeclSymbol idx)], ctx')

        dsmap = Map.fromList dsmap_list
        (dsmap_list, irctx') =
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
            make_list "sint64" (IntType Map.empty 64 Signed) $ ([], irctx)

instance Parent Module DeclSymbol String where
    get_child_map ((Module dsmap _ _), _) = dsmap
    add name ds (Module dsmap vmap sp, irctx) =
        let old_val = Map.lookup name dsmap
            dsmap' = Map.insert name ds dsmap
        in (old_val, (Module dsmap' vmap sp, irctx))

instance Parent Module Value String where
    get_child_map ((Module _ vmap _), _) = vmap
    add name ds (Module dsmap vmap sp, irctx) =
        let old_val = Map.lookup name vmap
            vmap' = Map.insert name ds vmap
        in (old_val, (Module dsmap vmap' sp, irctx))

instance DeclSpan Module where
    decl_span _ (Module _ _ sp) = Just sp
instance Describe Module where
    describe _ _ = "module being compiled"
