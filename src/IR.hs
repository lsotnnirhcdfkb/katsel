module IR
    ( Unit(..)
    ) where

import File

data Unit = Unit File Module

data Module = Module

data Type = Type

data DeclSymbol = DSModule Module
                | DSType Type
