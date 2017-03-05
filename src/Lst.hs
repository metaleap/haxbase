module Lst
(module Lst, L.groupBy, L.find, L.isPrefixOf, L.isInfixOf,
    L.isSuffixOf, L.lookup, L.span, L.break, L.dropWhile)
where

import Base
import qualified Fn

import qualified Data.List as L



begins  :: (Eq a)
        =>  a  ->  [a]
        ->  Bool
begins      item (item0:_)
    = item==item0
begins  _ _
    = False



count   ::  (Eq a)
        =>  a  ->  [a]
        ->  Int
count       item
    = countIf (==item)

countIf ::  (a->Bool)  ->  [a]
        ->  Int
countIf check
    = next 0 where
    next counter []             = counter
    next counter (this:rest)    | check this    = next (counter+1) rest
                                | otherwise     = next counter rest



crop    ::  Int  ->  Int  ->  [a]
        ->  [a]
crop        0 0         = id
crop        0 1         = dropLast 1
crop        0 end       = dropLast end
crop        1 0         = L.drop 1 -- `tail` could error out, one less worry
crop        start 0     = L.drop start
crop        start end   = (L.drop start) . (dropLast end)




-- for uses such as `crop` without (directly) taking the `length`
dropLast    ::  Int  ->  [a]
            ->  [a]
dropLast    0 list = list
dropLast    n list = zipWith const list (drop n list)
-- dropLast n = (@!n) . reverse . Data.List.inits
-- dropLast n l = l~>take (l~>length - n)


takeLast    ::  Int  ->  [a]
            ->  [a]
takeLast 0  = const []
-- takeLast 1 = (: []) . last
takeLast n  = ([] -|=) . (@?n) . reverse . L.tails



elemIn  ::  (Eq a)
        =>  [a]  ->  a
        ->  Bool
elemIn  = flip elem



indexed ::  [a]
        ->  [(Int , a)]
indexed = zip [0 .. ]



isInfixed   ::  (Eq a)
            =>  [a]  ->  [a]
            ->  Bool
isInfixed   = flip L.isInfixOf

isPrefixed  ::  (Eq a)
            =>  [a]  ->  [a]
            ->  Bool
isPrefixed  = flip L.isPrefixOf

isSuffixed  ::  (Eq a)
            =>  [a]  ->  [a]
            ->  Bool
isSuffixed  = flip L.isSuffixOf




join    ::  a  ->  [[a]]
        ->  [a]
join    delim
    = drop 1 . foldr (\value -> ((delim:value)++)) []


joined  ::  [a]  ->  [[a]]
        ->  [a]
joined = L.intercalate




lengthGEq   ::  Int  ->  [a]
            ->  Bool
lengthGEq   0 = const True
lengthGEq   n = has . drop (n - 1)

lengthGt    ::  Int  ->  [a]
            ->  Bool
lengthGt    0 = has
lengthGt    n = has . drop n




replace ::  (Eq a)
        =>  ([a] , [a])  ->  [a]
        ->  [a]
replace ([] , _)
    = id
replace (old , new)
    = replacer where
    oldlen = old~>length
    replacer []                     = []
    replacer listfrom@(cur:rest)    =
        if not (L.isPrefixOf old listfrom) then  cur : replacer rest
            else new ++ replacer (drop oldlen listfrom)


replaceAll  ::  (Eq a)
            =>  [([a],[a])]  ->  [a]
            ->  [a]
replaceAll  []
    = id
replaceAll  [oldnew]
    = replace oldnew
replaceAll  oldnewpairs
    = replacer where
    replacer [] = []
    replacer listfrom@(cur:rest) =
        case finder listfrom of
            Nothing -> cur : replacer rest
            Just (new , oldlen , _) -> new ++ (replacer (drop oldlen listfrom))
    finder listfrom = L.find match oldnews where match (_,_,m) = m listfrom
    oldnews = oldnewpairs >~ \ (old,new) -> (new , old~>length , L.isPrefixOf old)




shiftLeft   ::  [a]
            ->  [a]
shiftLeft   []          = []
shiftLeft   (from:rest) = rest ++ [from]


shuffleBasic    ::  [Int]  ->  [a]
                ->  [a]
shuffleBasic    [] list
    = list
shuffleBasic    _ []
    = []
shuffleBasic    (rnd:rnds) list
    | null left = shuffleBasic rnds right
    | otherwise = last left : shuffleBasic rnds ((L.init left) ++ right)
    where (left , right) = L.splitAt (rnd `mod` list~>length) list


shuffleExtra    ::  [Int]  ->  [a]
                ->  [a]
shuffleExtra    rnds@(_:_:_) list@(_:_:_)
    = reverseordont shifted where
        reverseordont = if 0 == (shiftby`mod`2) then L.reverse else id
        shifted = Fn.times shiftby shiftLeft shuffled
        shuffled = shuffleBasic rnds list
        shiftby = (rnds@!1) `mod` (list~>length - 1)
shuffleExtra    rnds list
    = shuffleBasic rnds list




splitBy ::  (a->Bool)  ->  [a]
        ->  [[a]]
splitBy check
    = foldr each [[]] where
        each _ []                   = []
        each item acc@(item0:rest)  | check item    = []:acc
                                    | otherwise     = (item:item0):rest

splitIt ::  Int  ->  [a]
        ->  ([a] , [a])
splitIt i list
    | i < 0     = (list , [])
    | otherwise = (take i list, drop (i+1) list)

splitOn ::  (Eq a)
        =>  a  ->  [a]
        ->  [[a]]
splitOn delim
    = splitBy (delim==)

splitOn1st  ::  (Eq a)
            =>  a  ->  [a]
            ->  ([a],[a])
splitOn1st  delim
    = (drop 1 <$>) . (L.break (==delim))




trim    ::  (a->Bool)  ->  [a]
        ->  [a]
trim fn = (trimEnd fn) . (trimStart fn)


trimEnd ::  (a->Bool)  ->  [a]
        ->  [a]
trimEnd = L.dropWhileEnd


trimEndEq   ::  (Eq a)
            =>  [a]  ->  [a]
            ->  [a]
trimEndEq   dropitems
    = trimEnd (`elem` dropitems)


trimStart   ::  (a->Bool)  ->  [a]
            ->  [a]
trimStart   = L.dropWhile


trimStartEq ::  (Eq a)
            =>  [a]  ->  [a]
            ->  [a]
trimStartEq dropitems
    = trimStart (`elem` dropitems)
