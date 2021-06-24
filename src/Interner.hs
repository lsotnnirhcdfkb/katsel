module Interner
    ( Interner
    , InternerIdx
    , new_interner
    , new_interner_with
    , all_interner_items
    , all_interner_idxs
    , add_to_interner
    , get_from_interner
    , replace_in_interner
    ) where

import Data.List(findIndex)

newtype Interner a = Interner [a]
newtype InternerIdx a = InternerIdx Int deriving Eq

new_interner :: Interner a
new_interner = Interner []

new_interner_with :: [a] -> Interner a
new_interner_with = Interner

all_interner_items :: Interner a -> [a]
all_interner_items (Interner items) = items

all_interner_idxs :: Interner a -> [InternerIdx a]
all_interner_idxs (Interner items) = take (length items) (InternerIdx <$> [0..])

add_to_interner :: (a -> a -> Bool) -> a -> Interner a -> (InternerIdx a, Interner a)
add_to_interner eq_fun item interner@(Interner items) =
    case findIndex (eq_fun item) items of
        Just idx -> (InternerIdx idx, interner)
        Nothing -> (InternerIdx $ length items, Interner $ items ++ [item])

get_from_interner :: InternerIdx a -> Interner a -> a
get_from_interner (InternerIdx idx) (Interner items) = items !! idx

replace_in_interner :: a -> InternerIdx a -> Interner a -> Interner a
replace_in_interner item (InternerIdx idx) (Interner items) =
    let (keep, _:keep2) = splitAt idx items
        items' = keep ++ item : keep2
    in Interner items'
