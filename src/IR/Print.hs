module IR.Print
    ( DSPrint(..)
    , VPrint(..)
    ) where

import {-# SOURCE #-} IR.IRCtx

class DSPrint d where
    ds_print :: IRCtx -> d -> String

class VPrint v where
    v_print :: IRCtx -> v -> String
