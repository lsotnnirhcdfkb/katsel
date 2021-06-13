module Mangle.TagName
    ( TagName
    , TagTagName
    , StrTagName

    , str_tag_name

    , tn'dsid
    , tn'vid
    , tn'name
    , tn'root
    , tn'next
    ) where

import qualified Data.Map as Map

data TagName k
    = TN'DSID
    | TN'VID
    | TN'Name
    | TN'Root
    | TN'Next
    deriving (Eq, Ord, Enum, Bounded)

data TagTagName
data StrTagName

tn'dsid, tn'vid, tn'root, tn'next :: TagName TagTagName
tn'dsid = TN'DSID
tn'vid = TN'VID
tn'root = TN'Root
tn'next = TN'Next

tn'name :: TagName StrTagName
tn'name = TN'Name

str_tag_name :: TagName k -> String
str_tag_name = (shortened Map.!) . full_str_name
    where
        full_str_name TN'DSID = "dsid"
        full_str_name TN'VID = "vid"
        full_str_name TN'Name = "name"
        full_str_name TN'Root = "root"
        full_str_name TN'Next = "next"

        -- longest common prefix
        lcp a b = takeWhile (uncurry (==)) (zip a b)

        all_names = enumFromTo minBound maxBound
        all_fulls = map full_str_name all_names
        shorten s = take (longest_lcp_length + 1) s
            where
                longest_lcp_length = maximum $ map (length . lcp s) (filter (s/=) all_fulls)
        shortened = Map.fromList $ map (\ n -> (n, shorten n)) all_fulls

