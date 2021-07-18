{-# LANGUAGE TemplateHaskell #-}

module IR.IRCtx
    ( IRCtx
    , ds_interner
    , v_interner
    , ds_child_list
    , v_child_list
    ) where

import Interner

import IR.ChildList

import IR.DeclSymbol
import IR.Value

import Control.Lens.TH (makeLenses)

type DeclSymbol' = DeclSymbol IRCtx
type Value' = Value IRCtx

data IRCtx
    = IRCtx
      { _ds_interner :: Interner DeclSymbol'
      , _v_interner :: Interner Value'
      , _ds_child_list :: ChildList (InternerIdx DeclSymbol') (InternerIdx DeclSymbol') String
      , _v_child_list :: ChildList (InternerIdx DeclSymbol') (InternerIdx Value') String
      }

$(makeLenses ''IRCtx)
