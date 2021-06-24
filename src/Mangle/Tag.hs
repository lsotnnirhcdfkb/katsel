module Mangle.Tag
    ( Tag(..)
    , str_tag
    ) where

import Mangle.TagName

import Numeric(showHex)
import Data.Char(isAscii, isAlpha, isDigit, ord)

data Tag = Tag (TagName TagTagName) [Tag] | StrTag (TagName StrTagName) String

str_tag :: Tag -> String
str_tag = ("_K"++) . str_tag'
    where
        str_tag' (Tag name children) = str_tag_name name ++ show (length children) ++ concatMap str_tag' children
        str_tag' (StrTag name val) =
            let val' = encode val
            in str_tag_name name ++ show (length val') ++ val'

encode :: String -> String
encode "" = ""
encode (c:cs) = encode_ch True c ++ concatMap (encode_ch False) cs
    where
        encode_ch first ch
            | isAlpha ch && isAscii ch = [ch]
            | isDigit ch && isAscii ch && not first = [ch]
            | ch == '_' = "__"
            | otherwise = "_u" ++ showHex (ord ch) "_"
