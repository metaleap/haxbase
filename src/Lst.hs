module Lst
(begins, join, joined, splitBy, splitOn, isInfixed, isPrefixed, isSuffixed,
    DL.find, DL.isInfixOf, DL.isPrefixOf, DL.isSuffixOf, DL.lookup)
where

import qualified Data.List as DL



begins :: (Eq a)=>  a  ->  [a]  ->  Bool
begins item (this:_) =
    item == this
begins _ _ =
    False



joined :: [a]  ->  [[a]]  ->  [a]
joined = DL.intercalate

join :: a  ->  [[a]]  ->  [a]
join delim =
    drop 1 . foldr (\value -> ((delim:value)++)) []



splitBy :: (a->Bool)  ->  [a]  ->  [[a]]
splitBy check =
    foldr each [[]] where
        each _ [] = []
        each item accum@(item0:rest)
            |(check item)= []:accum
            |(otherwise)= (item:item0):rest

splitOn :: (Eq a)=>  a  ->  [a]  ->  [[a]]
splitOn delim =
    splitBy (delim==)



isInfixed :: (Eq a)=>  [a]  ->  [a]  ->  Bool
isInfixed = flip DL.isInfixOf

isPrefixed :: (Eq a)=>  [a]  ->  [a]  ->  Bool
isPrefixed = flip DL.isPrefixOf

isSuffixed :: (Eq a)=>  [a]  ->  [a]  ->  Bool
isSuffixed = flip DL.isSuffixOf
