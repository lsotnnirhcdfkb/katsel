module Mangle.Tag
    ( Tag(..)
    , str_tag
    ) where

import Mangle.TagName

data Tag = Tag (TagName TagTagName) [Tag] | StrTag (TagName StrTagName) String

str_tag :: Tag -> String
str_tag = ("_K"++) . str_tag'
    where
        str_tag' (Tag name children) = str_tag_name name ++ show (length children) ++ concatMap str_tag' children
        str_tag' (StrTag name val) = str_tag_name name ++ show (length val) ++ "_" ++ val -- TODO: deal with non-ascii values
