{-# LANGUAGE MultiParamTypeClasses #-}

module IR.DeclSpan where

import Location

class DeclSpan ctx d where
    decl_span :: ctx -> d -> Maybe Span
