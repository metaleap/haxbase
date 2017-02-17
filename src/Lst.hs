module Lst
(join, splitBy, splitOn,
    DL.find, DL.isPrefixOf, DL.lookup)
where

import qualified Data.List as DL


join :: [a]  ->  [[a]]  ->  [a]
join = DL.intercalate


splitBy :: (a->Bool)  ->  [a]  ->  [[a]]
splitBy check =
    foldr each [[]] where
        each _ [] = []
        each item accum@(item0:rest)
            |(check item)= []:accum
            |(otherwise)= (item:item0):rest

splitOn :: (Eq a) => a  ->  [a]  ->  [[a]]
splitOn delim =
    splitBy (delim==)
