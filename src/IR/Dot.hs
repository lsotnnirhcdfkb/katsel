{-# LANGUAGE FlexibleContexts #-}

module IR.Dot
    ( dot_mod
    ) where

import IR.Parent

import IR.DeclSymbol
import IR.Value

import IR.Module
import IR.Type

import IR.Function
import IR.ConstFunctionPointer

import IR.IRCtx

import Interner

import Data.List(foldl', nub)
import Data.Map(Map)
import qualified Data.Map as Map

data Graph = Graph Node Node
data Node
    = Node String [(FieldColor, String, Either String Node)]
    | Cluster String [Node]
    deriving (Eq, Ord)
data FieldColor = NoColor | DSColor | VColor deriving (Eq, Ord)

make_node_field :: String -> Node -> (FieldColor, String, Either String Node)
make_node_field name node = (NoColor, name, Right node)
make_str_field :: String -> String -> (FieldColor, String, Either String Node)
make_str_field name val = (NoColor, name, Left val)

dot_mod :: IRCtx -> Module -> String
dot_mod irctx m = str_graph $
    Graph
        (dot_irctx irctx)
        (Cluster "module" $ dotted_m : nub (rec_find_nodes dotted_m))
    where
        dotted_m = dot_ds irctx (DeclSymbol m)

rec_find_nodes :: Node -> [Node]
rec_find_nodes (Node _ nodes) = [n | (_, _, Right n) <- nodes]
rec_find_nodes (Cluster _ _) = []

-- str_node {{{1
data NodeIDMap = NodeIDMap Int (Map Node String)

get_node_id :: NodeIDMap -> Node -> (String, NodeIDMap)
get_node_id orig_node_id_map@(NodeIDMap cur_id node_map) node =
    case Map.lookup node node_map of
        Just i -> (i, orig_node_id_map)
        Nothing ->
            let new_id =
                    case node of
                        Cluster _ _ -> "cluster_node" ++ show cur_id
                        Node _ _ -> "node" ++ show cur_id
            in (new_id, NodeIDMap (cur_id + 1) (Map.insert node new_id node_map))

str_graph :: Graph -> String
str_graph (Graph node1 node2) = "strict digraph { node [shape=plain];" ++ node1_strd ++ node2_strd ++ " }"
    where
        node_map = NodeIDMap 0 Map.empty
        (node1_strd, node_map') = str_node node_map node1
        (node2_strd, _) = str_node node_map' node2

str_node :: NodeIDMap -> Node -> (String, NodeIDMap)
str_node node_ids node@(Node name fields) =
    let (nodeid, node_ids') = get_node_id node_ids node

        make_row (field_id, (field_color, field_key, field_val)) =
            "<tr><td " ++ strd_color ++ ">" ++ escape field_key ++ "</td>" ++
            (case field_val of
                Left str -> "<td>" ++ escape str ++ "</td>"
                Right _ -> "<td port=\"port" ++ show field_id ++ "\" width=\"25\"></td>"
            ) ++ "</tr>"
            where
                strd_color = case field_color of
                    NoColor -> ""
                    DSColor -> "color=\"blue\""
                    VColor -> "color=\"green\""

        make_connection nids (field_id, (_, _, Right pointee)) =
            let (connection_to_id, nids') = get_node_id nids pointee
            in (nodeid ++ ":port" ++ show field_id ++ " -> " ++ connection_to_id ++ ";", nids')
        make_connection nids _ = ("", nids)
        chain_connections (acc, nids) connection =
            let (cur_conn, nids') = make_connection nids connection
            in (acc ++ cur_conn, nids')

        numbered_fields :: [(Int, (FieldColor, String, Either String Node))]
        numbered_fields = zip [0..] fields

        name_row = "<tr><td colspan=\"2\">" ++ escape name ++ "</td></tr>"
        html_rows = name_row ++ concatMap make_row numbered_fields

        node_def = nodeid ++ " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\">" ++ html_rows ++ "</table>>];"
        (connections, node_ids'') = foldl' chain_connections ([], node_ids') numbered_fields

    in (node_def ++ connections, node_ids'')

str_node node_ids node@(Cluster name nodes) =
    let (nodeid, node_ids') = get_node_id node_ids node

        str_nodes nids [] = ([], nids)
        str_nodes nids (cur_node:more) =
            let (strd_node, nids') = str_node nids cur_node
                (more_strd, nids'') = str_nodes nids' more
            in (strd_node ++ more_strd, nids'')

        (strd_nodes, node_ids'') = str_nodes node_ids' nodes

    in ("subgraph " ++ nodeid ++ " { graph [label=\"" ++ escape name ++ "\"];" ++ strd_nodes ++ " };", node_ids'')

escape :: String -> String
escape =
    replace '"'  "&quot;" .
    replace '\'' "&apos;" .
    replace '<'  "&lt;" .
    replace '>'  "&gt;" .
    replace '&'  "&amp;"

replace :: Eq a => a -> [a] -> [a] -> [a]
replace _ _ [] = []
replace x y (ch:more)
    | ch == x = y ++ more'
    | otherwise = ch : more'
    where
        more' = replace x y more
-- irctx {{{1
dot_irctx :: IRCtx -> Node
dot_irctx irctx =
    Cluster "irctx"
        [ Cluster "type interner" $ map (nodify dot_type irctx) types
        , Cluster "function interner" $ map (dot_fun irctx) function
        ]
    where
        type_interner = get_type_interner irctx
        types = all_interner_items type_interner

        function_interner = get_function_interner irctx
        function = all_interner_items function_interner
-- declsymbols {{{1
dot_ds :: IRCtx -> DeclSymbol -> Node
dot_ds irctx = apply_to_ds (nodify dot_mod' irctx) (nodify dot_tyidx irctx)

nodify :: (Parent a DeclSymbol String, Parent a Value String) => (IRCtx -> a -> (String, [(FieldColor, String, Either String Node)])) -> IRCtx -> a -> Node
nodify f irctx a = Node name (fields ++ ds_children ++ v_children)
    where
        (name, fields) = f irctx a

        ds_children = map (\ (k, v) -> (DSColor, k, v)) $ Map.toAscList $ Map.map (Right . dot_ds irctx) (get_child_map (a, irctx))
        v_children = map (\ (k, v) -> (VColor, k, v)) $ Map.toAscList $ Map.map (Right . dot_v irctx) (get_child_map (a, irctx))

dot_mod' :: IRCtx -> Module -> (String, [(FieldColor, String, Either String Node)])
dot_mod' _ _ = ("mod", [])

dot_tyidx :: IRCtx -> InternerIdx Type -> (String, [(FieldColor, String, Either String Node)])
dot_tyidx irctx = apply_to_tyidx (dot_type irctx) irctx
-- values {{{1
dot_v :: IRCtx -> Value -> Node
dot_v irctx = apply_to_v (dot_fun_ptr irctx)

dot_fun_ptr :: IRCtx -> ConstFunctionPointer -> Node
dot_fun_ptr irctx fptr = Node "function pointer" [make_node_field "pointee" (dot_fun irctx $ get_fptr_pointee irctx fptr)]
-- types {{{1
dot_type :: IRCtx -> Type -> (String, [(FieldColor, String, Either String Node)])
dot_type _ (FloatType _ size) = ("float type", [make_str_field "size" (show size)])
dot_type _ (IntType _ size signedness) =
    ("int type",
        [ make_str_field "size" (show size)
        , make_str_field "signedness" $
              case signedness of
                  Unsigned -> "unsigned"
                  Signed -> "signed"
        ])
dot_type _ (CharType _) = ("char type", [])
dot_type _ (BoolType _) = ("bool type", [])
dot_type irctx (FunctionPointerType _ ret params) =
    ("function pointer type", [make_node_field "return type" (nodify dot_tyidx irctx ret)] ++ zipWith dot_param ([0..] :: [Int]) params)
    where
        dot_param i ty = make_node_field ("parameter type " ++ show i) (nodify dot_tyidx irctx ty)
dot_type _ (UnitType _) = ("unit type", [])
-- functions {{{1
dot_fun :: IRCtx -> Function -> Node
dot_fun _ _ = Node "function" [] -- TODO
