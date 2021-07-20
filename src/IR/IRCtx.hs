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

import SimpleLens

import Pool

import IR.ChildList

import IR.DeclSymbol
import IR.Value

type DeclSymbol' = DeclSymbol IRCtx
type Value' = Value IRCtx

data IRCtx
    = IRCtx
      { _ds_pool :: Pool DeclSymbol'
      , _v_pool :: Pool Value'
      , _ds_child_list :: ChildList (PoolIdx DeclSymbol') (PoolIdx DeclSymbol') String
      , _v_child_list :: ChildList (PoolIdx DeclSymbol') (PoolIdx Value') String
      }

ds_pool :: Lens IRCtx (Pool DeclSymbol')
ds_pool = Lens _ds_pool (\ a b -> a { _ds_pool = b })

v_pool :: Lens IRCtx (Pool Value')
v_pool = Lens _v_pool (\ a b -> a { _v_pool = b })

ds_child_list :: Lens IRCtx (ChildList (PoolIdx DeclSymbol') (PoolIdx DeclSymbol') String)
ds_child_list = Lens _ds_child_list (\ a b -> a { _ds_child_list = b })

v_child_list :: Lens IRCtx (ChildList (PoolIdx DeclSymbol') (PoolIdx Value') String)
v_child_list = Lens _v_child_list (\ a b -> a { _v_child_list = b })
