module Mangle
    ( MangledName
    , mangled_str
    , mangle_dsid
    , mangle_vid
    ) where

import qualified IR

import Data.Char(toUpper, toLower)

data MangledName = MangledName { mangled_str :: String }
data MangleTag = Tag String [MangleTag] | StrTag String String

str_tag :: MangleTag -> MangledName
str_tag t = MangledName $ "_K" ++ str_tag' t
    where
        str_tag' (Tag name children) = str_tag_name name ++ show (length children) ++ concatMap str_tag' children
        str_tag' (StrTag name val) = "s" ++ str_tag_name name ++ show (length val) ++ "_" ++ val -- TODO: deal with non-ascii values

str_tag_name :: String -> String
str_tag_name n = map toUpper (init n) ++ [toLower (last n)]

mangle_dsid :: IR.DSIRId d -> MangledName
mangle_dsid dsid = str_tag $ Tag "dsid" [segments_tag "root" $ IR.dsid_segments dsid]
        
mangle_vid :: IR.VIRId v -> MangledName
mangle_vid vid = str_tag $ Tag "vid" [segments_tag "root" $ IR.vid_segments vid]

segments_tag :: String -> [String] -> MangleTag
segments_tag name [x] = Tag name [StrTag "name" x]
segments_tag name (cur:more) = Tag name [StrTag "name" cur, segments_tag "next" more]
segments_tag _ [] = error "cannot make tag for 0 segments"
