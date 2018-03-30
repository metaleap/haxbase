module Str
    ( module Str
    , L.unlines
    , L.lines
    ) where

import Base
import qualified Lst

import qualified Data.Char
import qualified Data.List as L
import qualified Text.Printf
import qualified Text.Read

type Pairs = [(String, String)]

formatWithList ::
       (Text.Printf.PrintfType a, Text.Printf.PrintfArg t)
    => String
    -> [t]
    -> Maybe a
formatWithList text vals
    -- silly hack for passing list-of-user-args into this annoyingly-variadic built-in
    -- hardcoded "support for up to 6"  =)
    | num == 1 = Just $ a1 (Text.Printf.printf text) args
    | num == 2 = Just $ a2 (Text.Printf.printf text) args
    | num == 3 = Just $ a3 (Text.Printf.printf text) args
    | num == 4 = Just $ a4 (Text.Printf.printf text) args
    | num == 5 = Just $ a5 (Text.Printf.printf text) args
    | num == 6 = Just $ a6 (Text.Printf.printf text) args
    | otherwise = Nothing
  where
    args = take num (cycle vals)
    num = c 0 text
    c n ('%':'s':m) = c (n + 1) m
    c n ('%':'v':m) = c (n + 1) m
    c n (_:m) = c n m
    c n [] = n
    a1 p [_1] = p _1
    a1 _ _ = undefined
    a2 p [_1, _2] = p _1 _2
    a2 _ _ = undefined
    a3 p [_1, _2, _3] = p _1 _2 _3
    a3 _ _ = undefined
    a4 p [_1, _2, _3, _4] = p _1 _2 _3 _4
    a4 _ _ = undefined
    a5 p [_1, _2, _3, _4, _5] = p _1 _2 _3 _4 _5
    a5 _ _ = undefined
    a6 p [_1, _2, _3, _4, _5, _6] = p _1 _2 _3 _4 _5 _6
    a6 _ _ = undefined

multiLinedChunksBy :: (String -> String -> Bool) -> String -> [String]
multiLinedChunksBy doesntbeginchunk =
    (L.unlines <$>) .
    Lst.groupBy doesntbeginchunk .
    Lst.dropWhile (\x -> doesntbeginchunk x "") . L.lines

multiLinedChunksByIndent :: String -> [String]
multiLinedChunksByIndent = multiLinedChunksBy $ const . doesntbeginchunk
  where
    doesntbeginchunk "" = True
    doesntbeginchunk (c:_) = Data.Char.isSpace c

teaser :: Int -> String -> String
teaser maxlen str =
    if s == str
        then s
        else (s ++ "...")
  where
    s = take maxlen str

toLower :: String -> String
toLower = fmap Data.Char.toLower

toUpper :: String -> String
toUpper = fmap Data.Char.toUpper

trim :: String -> String
trim = Lst.trim Data.Char.isSpace

trimBoth :: (String, String) -> (String, String)
trimBoth = duo (trim, trim)

trimEnd :: String -> String
trimEnd = Lst.trimEnd Data.Char.isSpace

trimStart :: String -> String
trimStart = Lst.trimStart Data.Char.isSpace

trimSpaceOr :: [Char] -> String -> String
trimSpaceOr dropitems = Lst.trim (Data.Char.isSpace |.| Lst.elemIn dropitems)

tryParse :: (Read r) => String -> Maybe r
tryParse = Text.Read.readMaybe

tryParseNonNull :: (Read r) => r -> r -> (String -> String) -> String -> r
tryParseNonNull nullval errval toparsestr str =
    if (null str)
        then nullval
        else errval -|= (Text.Read.readMaybe $ toparsestr str)

tryParseOr :: (Read r) => r -> String -> r
tryParseOr defval = (defval -|=) . Text.Read.readMaybe
