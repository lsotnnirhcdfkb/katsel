module LowerAST(lowerCU) where

import qualified IR
import qualified AST

lowerCU :: AST.LDCU -> Maybe IR.Unit
lowerCU _ = error "TODO"
