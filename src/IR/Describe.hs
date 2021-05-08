module IR.Describe where

class Describe d where
    describe :: d -> String
