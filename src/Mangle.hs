module Mangle
    ( MangledName
    , mangled_str
    , mangle_dsid
    , mangle_vid
    , mangle_fun
    ) where

import qualified IR

import Interner

import Mangle.TagName
import Mangle.Tag

data MangledName = MangledName { mangled_str :: String }

to_mn :: Tag -> MangledName
to_mn = MangledName . str_tag

mangle_dsid :: IR.DSIRId d -> MangledName
mangle_dsid dsid = to_mn $ Tag tn'dsid [segments_tag tn'root $ IR.dsid_segments dsid]

mangle_vid :: IR.VIRId v -> MangledName
mangle_vid vid = to_mn $ Tag tn'vid [segments_tag tn'root $ IR.vid_segments vid]

segments_tag :: TagName TagTagName -> [String] -> Tag
segments_tag name [x] = Tag name [StrTag tn'name x]
segments_tag name (cur:more) = Tag name [StrTag tn'name cur, segments_tag tn'next more]
segments_tag _ [] = error "cannot make tag for 0 segments"

mangle_fun :: InternerIdx IR.Function -> MangledName
mangle_fun fidx = to_mn $ Tag tn'funidx [StrTag tn'idx (show fidx)]
