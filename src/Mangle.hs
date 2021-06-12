module Mangle
    ( MangledName
    , mangled_str
    , mangle_dsid
    , mangle_vid
    ) where

import qualified IR

import qualified Data.Map as Map

data MangledName = MangledName { mangled_str :: String }
data MangleTag = Tag TagName [MangleTag] | StrTag TagName String

data TagName
    = TN'DSID
    | TN'VID
    | TN'Name
    | TN'Root
    | TN'Next
    deriving (Eq, Ord, Enum, Bounded)

str_tag :: MangleTag -> MangledName
str_tag t = MangledName $ "_K" ++ str_tag' t
    where
        str_tag' (Tag name children) = str_tag_name name ++ show (length children) ++ concatMap str_tag' children
        str_tag' (StrTag name val) = "s" ++ str_tag_name name ++ show (length val) ++ "_" ++ val -- TODO: deal with non-ascii values

str_tag_name :: TagName -> String
str_tag_name = (shortened Map.!)
    where
        full_str_name :: TagName -> String
        full_str_name TN'DSID = "DSID"
        full_str_name TN'VID = "VID"
        full_str_name TN'Name = "NAME"
        full_str_name TN'Root = "ROOT"
        full_str_name TN'Next = "NEXT"

        -- longest common prefix
        lcp a b = takeWhile (uncurry (==)) (zip a b)

        all_names = enumFromTo minBound maxBound
        all_fulls = map full_str_name all_names
        shorten s =
            take (longest_lcp_length + 1) s
            where
                longest_lcp_length = maximum $ map (length . lcp s) (filter (s/=) all_fulls)
        shortened = Map.fromList $ map (\ n -> (n, shorten $ full_str_name n)) all_names

-- map toUpper (init n) ++ [toLower (last n)]

mangle_dsid :: IR.DSIRId d -> MangledName
mangle_dsid dsid = str_tag $ Tag TN'DSID [segments_tag TN'Root $ IR.dsid_segments dsid]

mangle_vid :: IR.VIRId v -> MangledName
mangle_vid vid = str_tag $ Tag TN'VID [segments_tag TN'Root $ IR.vid_segments vid]

segments_tag :: TagName -> [String] -> MangleTag
segments_tag name [x] = Tag name [StrTag TN'Name x]
segments_tag name (cur:more) = Tag name [StrTag TN'Name cur, segments_tag TN'Next more]
segments_tag _ [] = error "cannot make tag for 0 segments"
