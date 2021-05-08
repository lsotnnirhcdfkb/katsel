module IR.DeclSpan where

import Location

class DeclSpan h where
    decl_span :: h -> Maybe Span
