module IR.IRCtx
    ( IRCtx
    , new_irctx
    , get_ty_irctx
    , resolve_float32
    , resolve_float64
    , resolve_uint8
    , resolve_uint16
    , resolve_uint32
    , resolve_uint64
    , resolve_sint8
    , resolve_sint16
    , resolve_sint32
    , resolve_sint64
    , resolve_generic_float
    , resolve_generic_int
    , resolve_char
    , resolve_bool
    , resolve_unit
    , apply_to_tyidx
    , get_type_interner
    ) where

import IR.TypeStuffAndIRCtx.Stuff
