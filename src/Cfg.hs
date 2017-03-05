{-# LANGUAGE NamedFieldPuns #-}
module Cfg
where

import Base
import qualified Lst
import qualified Map
import qualified Str


data Syntax
    = Using {
        delimHeaderBody :: Char,
        delimNameMeta :: Char,
        delimsSubst :: (String , String)
    }


newtype Intermediate
    = Chunks (Map.Map String (String , String))
    deriving Show


data Config a
    = Loaded {
        allNames :: [String],
        get :: (String->Maybe a),
        namesWhereMeta ::  (String->Bool)  ->  [String]
    }


usingDefaultSyntax  ::  Syntax
usingDefaultSyntax
    = Using{ delimHeaderBody = '=' , delimNameMeta = ':' , delimsSubst = ("{{","}}") }




chunksFrom  ::  Syntax  ->  String
            ->  Intermediate
chunksFrom      Using{ delimHeaderBody , delimNameMeta , delimsSubst } src
    = Chunks$ Map.fromList chunksfinal
    where
    chunksfinal = chunksreplaced >~ foreach
    foreach (header,body) =
        (name , (meta , body)) where (name,meta) = splitnamemeta header

    chunksreplaced = chunksraw >~ (>~ Lst.replaceAll tmplrepls)
    chunksraw = Str.multiLinedChunksByIndent src
        >~ Lst.splitOn1st delimHeaderBody
    tmplrepls = chunksraw >~ \ (header , body) -> (tmplreplname header , body)
    tmplreplname = (delimsSubst~>fst ++) . (++ delimsSubst~>snd) . fst . splitnamemeta
    splitnamemeta = Lst.splitOn1st delimNameMeta



load    ::  (Read r)
        =>  (String->(String->String))  ->  Intermediate
        ->  Config r
load        getreadstrmaker (Chunks strictmap)
    = Loaded { allNames = allnames , get = fetch , namesWhereMeta = nameswheremeta }
    where
    allnames = Map.keys strictmap
    nameswheremeta metacheck =
        Map.keys$ Map.filter (metacheck.fst) strictmap
    fetch name =
        case Map.lookup name strictmap of
            Nothing -> Nothing
            Just ("",body) -> Str.tryParse body
            Just (meta,body) -> Str.tryParse ((getreadstrmaker meta) body)
