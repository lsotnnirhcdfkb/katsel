{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Typed
    ( Typed(..)
    ) where

-- 'tyr' for 'type representation'
class Typed ctx tyr v where
    type_of :: ctx -> v -> tyr
