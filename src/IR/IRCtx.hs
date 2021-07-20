{-# LANGUAGE TemplateHaskell #-}

module IR.IRCtx
    ( IRCtx

    , DeclSymbol'
    , Value'

    , ds_pool
    , v_pool
    , ds_child_list
    , v_child_list
    ) where

import Pool

import IR.ChildList

import IR.DeclSymbol
import IR.Value

import Control.Lens.TH (makeLenses)

type DeclSymbol' = DeclSymbol IRCtx
type Value' = Value IRCtx

data IRCtx
    = IRCtx
      { _ds_pool :: Pool DeclSymbol'
      , _v_pool :: Pool Value'
      , _ds_child_list :: ChildList (PoolIdx DeclSymbol') (PoolIdx DeclSymbol') String
      , _v_child_list :: ChildList (PoolIdx DeclSymbol') (PoolIdx Value') String
      }

makeLenses ''IRCtx
