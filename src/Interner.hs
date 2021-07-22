module Interner
    ( Interner
    , InternerIdx
    , new_interner
    , new_interner_with
    , interner_items
    , interner_idxs
    , get_from_interner
    , resolve_interner_idx

    , tests
    ) where

import Test

import Data.List (elemIndex)

newtype Interner a = Interner [a]
newtype InternerIdx a = InternerIdx Int deriving (Eq, Ord)

instance Show (InternerIdx a) where
    show (InternerIdx i) = show i

new_interner :: Interner a
new_interner = Interner []

new_interner_with :: [a] -> Interner a
new_interner_with = Interner

interner_items :: Interner a -> [a]
interner_items (Interner items) = items

interner_idxs :: Interner a -> [InternerIdx a]
interner_idxs (Interner items) = take (length items) (InternerIdx <$> [0..])

get_from_interner :: Eq a => a -> Interner a -> (InternerIdx a, Interner a)
get_from_interner item interner@(Interner items) =
    case elemIndex item items of
        Just idx -> (InternerIdx idx, interner)
        Nothing -> (InternerIdx $ length items, Interner $ items ++ [item])

resolve_interner_idx :: InternerIdx a -> Interner a -> a
resolve_interner_idx (InternerIdx idx) (Interner items) = items !! idx

tests :: Test
tests = DescribeModule "Interner"
    [ DescribeFunction "new_interner"
        [ ItCan "create an empty interner" $
            case new_interner of
                Interner [] -> pass_test
                _ -> fail_test
        ]

    , DescribeFunction "new_interner_with"
        [ ItCan "create a new interner with the given elements" $
            case new_interner_with [1 :: Int, 2, 3] of
                Interner [1, 2, 3] -> pass_test
                _ -> fail_test
        ]

    , DescribeFunction "interner_items"
        [ ItCan "get all the items in a interner" $
            case interner_items $ new_interner_with [1 :: Int, 2, 3] of
                [1, 2, 3] -> pass_test
                _ -> fail_test
        ]

    , DescribeFunction "interner_idxs"
        [ ItCan "get all the indexes in a interner" $
            case interner_idxs $ new_interner_with [1 :: Int, 2, 3] of
                [InternerIdx 0, InternerIdx 1, InternerIdx 2] -> pass_test
                _ -> fail_test
        ]

    , DescribeFunction "get_from_interner"
        [ ItCan "get an item from an empty interner" $
            let interner = new_interner
                res = get_from_interner (1 :: Int) interner
            in case res of
                (InternerIdx 0, Interner [1]) -> pass_test
                _ -> fail_test

        , ItCan "get an item from a non-empty interner" $
            let interner = new_interner_with [1, 2, 3]
                res = get_from_interner (5 :: Int) interner
            in case res of
                (InternerIdx 3, Interner [1, 2, 3, 5]) -> pass_test
                _ -> fail_test

        , ItCan "return an old item if it is already in the interner" $
            let interner = new_interner_with [1, 2, 3]
                res = get_from_interner (3 :: Int) interner
            in case res of
                (InternerIdx 2, Interner [1, 2, 3]) -> pass_test
                _ -> fail_test
        ]

    , DescribeFunction "resolve_interner_idx" $
        [ ItCan "return the item at an index" $
            let interner = new_interner_with [1 :: Int, 2, 3]
                idx = InternerIdx 1
            in pass_if $ resolve_interner_idx idx interner == 2
        ]
    ]
