module IR.ChildList
    ( ChildList
    , new_child_list
    , get_child_map
    , get
    , add_replace
    , add_noreplace

    , tests
    ) where

import Test

import Data.Map (Map)
import qualified Data.Map as Map

data ChildList p c i = ChildList { un_child_list :: Map p (Map i c) }

new_child_list :: ChildList p i c
new_child_list = ChildList Map.empty

get_child_map :: (Ord p) => p -> ChildList p c i -> Map i c
get_child_map p (ChildList cl) = Map.findWithDefault Map.empty p cl

get :: (Ord p, Ord i) => p -> i -> ChildList p c i -> Maybe c
get p i = Map.lookup i . get_child_map p

add :: (Ord p, Ord i) => p -> i -> c -> ChildList p c i -> (Maybe c, ChildList p c i)
add p i c cl@(ChildList clm) =
    let parent_map = get_child_map p cl
        parent_map' = Map.insert i c parent_map

        old_child = Map.lookup i parent_map

        clm' = Map.insert p parent_map' clm

    in (old_child, ChildList clm')

add_replace :: (Ord p, Ord i) => p -> i -> c -> ChildList p c i -> ChildList p c i
add_replace cl p i c = snd $ add cl p i c

add_noreplace :: (Ord p, Ord i) => p -> i -> c -> ChildList p c i -> Either c (ChildList p c i)
add_noreplace cl p i c =
    case add cl p i c of
        (Just old, _) -> Left old
        (Nothing, p') -> Right p'

tests :: Test
tests =
    let cl :: ChildList Int Int String
        cl = ChildList $
            Map.fromList
                [ (2, Map.fromList [("A", 3), ("B", 4)])
                ]
    in DescribeModule "IR.ChildList"
    [ DescribeFunction "new_child_list"
        [ ItCan "create an empty child list" $
            pass_if $ un_child_list (new_child_list :: ChildList Int Int String) == Map.empty
        ]

    , DescribeFunction "get_child_map"
        [ ItCan "get the child map of a parent" $
            pass_if $ get_child_map 2 cl == Map.fromList [("A", 3), ("B", 4)]

        , ItCan "return an empty child map if the parent is not part of the map" $
            pass_if $ get_child_map 3 cl == Map.empty
        ]

    , DescribeFunction "get"
        [ ItCan "return the child of a parent" $
            pass_if $ get 2 "A" cl == Just 3

        , ItCan "return Nothing if the parent does not have a child of that name" $
            pass_if $ get 2 "C" cl == Nothing
        ]

    , DescribeFunction "add"
        [ ItCan "add a child to a parent" $
            let (old, cl') = add 2 "C" 5 cl
            in pass_if $ old == Nothing &&
                un_child_list cl' ==
                    Map.fromList
                        [ (2, Map.fromList [("A", 3), ("B", 4), ("C", 5)])
                        ]

        , ItCan "add a child to a parent that is not in the map" $
            let (old, cl') = add 3 "A" 4 cl
            in pass_if $ old == Nothing &&
                un_child_list cl' ==
                    Map.fromList
                        [ (2, Map.fromList [("A", 3), ("B", 4)])
                        , (3, Map.fromList [("A", 4)])
                        ]

        , ItCan "return the old child and replace it" $
            let (old, cl') = add 2 "A" 5 cl
            in pass_if $ old == Just 3 &&
                un_child_list cl' ==
                    Map.fromList
                        [ (2, Map.fromList [("A", 5), ("B", 4)])
                        ]
        ]

    , DescribeFunction "add_replace"
        [ ItCan "add a child to a parent" $
            let cl' = add_replace 2 "C" 5 cl
            in pass_if $
                un_child_list cl' ==
                    Map.fromList
                        [ (2, Map.fromList [("A", 3), ("B", 4), ("C", 5)])
                        ]

        , ItCan "add a child to a parent that is not already in the map" $
            let cl' = add_replace 3 "C" 5 cl
            in pass_if $
                un_child_list cl' ==
                    Map.fromList
                        [ (2, Map.fromList [("A", 3), ("B", 4)])
                        , (3, Map.fromList [("C", 5)])
                        ]

        , ItCan "add a child to a parent and replace the old child" $
            let cl' = add_replace 2 "A" 5 cl
            in pass_if $
                un_child_list cl' ==
                    Map.fromList
                        [ (2, Map.fromList [("A", 5), ("B", 4)])
                        ]
        ]

    , DescribeFunction "add_noreplace"
        [ ItCan "add a child to a parent" $
            let cl' = add_noreplace 2 "C" 5 cl
            in pass_if $
                case cl' of
                    Right cl''
                        | un_child_list cl'' ==
                            Map.fromList
                                [ (2, Map.fromList [("A", 3), ("B", 4), ("C", 5)])
                                ]
                            -> True
                    _ -> False

        , ItCan "add a child to a parent that is not already in the map" $
            let cl' = add_noreplace 3 "C" 5 cl
            in pass_if $
                case cl' of
                    Right cl''
                        | un_child_list cl'' ==
                            Map.fromList
                                [ (2, Map.fromList [("A", 3), ("B", 4)])
                                , (3, Map.fromList [("C", 5)])
                                ]
                            -> True
                    _ -> False

        , ItCan "return the old child if there is one already there" $
            let cl' = add_noreplace 2 "A" 5 cl
            in pass_if $
                case cl' of
                    Left 3 -> True
                    _ -> False
        ]
    ]
