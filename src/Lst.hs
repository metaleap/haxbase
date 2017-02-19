module Lst
(module L, module Lst)
where

import Data.List as L



begins :: (Eq a)=>  a  ->  [a]  ->  Bool
begins item (this:_) =
    item == this
begins _ _ =
    False



elemIn :: (Eq a)=>  [a]  ->  a  ->  Bool
elemIn = flip elem



isInfixed :: (Eq a)=>  [a]  ->  [a]  ->  Bool
isInfixed = flip L.isInfixOf

isPrefixed :: (Eq a)=>  [a]  ->  [a]  ->  Bool
isPrefixed = flip L.isPrefixOf

isSuffixed :: (Eq a)=>  [a]  ->  [a]  ->  Bool
isSuffixed = flip L.isSuffixOf



joined :: [a]  ->  [[a]]  ->  [a]
joined = L.intercalate

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



trim :: (a->Bool)  ->  [a]  ->  [a]
trim fn =
    (trimEnd fn) . (trimStart fn)

trimEnd :: (a->Bool)  ->  [a]  ->  [a]
trimEnd = L.dropWhileEnd

trimEndEq ::  (Eq a)=>  [a]  ->  [a]  ->  [a]
trimEndEq dropitems = trimEnd (`elem` dropitems)

trimStart :: (a->Bool)  ->  [a]  ->  [a]
trimStart = L.dropWhile

trimStartEq :: (Eq a)=>  [a]  ->  [a]  ->  [a]
trimStartEq dropitems = trimStart (`elem` dropitems)
