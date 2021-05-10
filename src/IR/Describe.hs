module IR.Describe where

-- TODO: import IR.IRCtx, fix circular import

class Describe d where
    describe :: d -> String
