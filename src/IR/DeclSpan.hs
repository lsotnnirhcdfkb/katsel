module IR.DeclSpan where

import {-# SOURCE #-} IR.IRCtx

import Location

class DeclSpan h where
    decl_span :: IRCtx -> h -> Maybe Span
