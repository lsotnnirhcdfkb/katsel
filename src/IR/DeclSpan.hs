module IR.DeclSpan where

-- TODO: import IR.IRCtx, fix circular import
import Location

class DeclSpan h where
    decl_span :: h -> Maybe Span
