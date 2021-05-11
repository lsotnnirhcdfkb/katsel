module IR.Describe where

import {-# SOURCE #-} IR.IRCtx

class Describe d where
    describe :: IRCtx -> d -> String
