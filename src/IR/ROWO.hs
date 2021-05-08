{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module IR.ROWO
    ( IRRO
    , IRWO
    , make_irro
    , make_irwo
    , unirro
    , unirwo
    , ro_to_wo
    , ro_cast
    , wo_cast
    ) where

import IR.Parent

import qualified Data.Map as Map

import Data.Typeable(Typeable, cast)

newtype IRRO a = IRRO { unirro :: a }
newtype IRWO a = IRWO { unirwo :: a }

make_irro :: a -> IRRO a
make_irro = IRRO

make_irwo :: a -> IRWO a
make_irwo = IRWO

ro_to_wo :: IRRO a -> IRWO a
ro_to_wo (IRRO a) = IRWO a

instance (Ord i, ParentR a c i) => ParentR (IRRO a) (IRRO c) i where
    get_child_map (IRRO ro) = Map.map IRRO $ get_child_map ro

instance (Ord i, ParentW a c i) => ParentW (IRWO a) (IRWO c) i where
    add ind (IRWO child) (IRWO wo) =
        let (replaced, added) = add ind child wo
        in (IRWO <$> replaced, IRWO added)

ro_cast :: (Typeable a, Typeable b) => IRRO a -> Maybe (IRRO b)
ro_cast (IRRO a) = IRRO <$> cast a
wo_cast :: (Typeable a, Typeable b) => IRWO a -> Maybe (IRWO b)
wo_cast (IRWO a) = IRWO <$> cast a
