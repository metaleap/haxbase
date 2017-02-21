{-# LANGUAGE OverloadedStrings #-}
module Txt
(module Txt)
where

import Base
import qualified Lst

import qualified Data.Char
import qualified Data.Text as T




isInfixed   ::  T.Text  ->  T.Text
            ->  Bool
isInfixed   = flip T.isInfixOf


isPrefixed  ::  T.Text  ->  T.Text
            ->  Bool
isPrefixed  = flip T.isPrefixOf


isSuffixed  ::  T.Text  ->  T.Text
            ->  Bool
isSuffixed  = flip T.isSuffixOf




joined  ::  T.Text  ->  [T.Text]
        ->  T.Text
joined  = T.intercalate




multiLinedChunksBy  ::  (T.Text->T.Text->Bool)  ->  T.Text
                    ->  [T.Text]
multiLinedChunksBy  doesntbeginchunk
    = (T.unlines <$>) . Lst.groupBy doesntbeginchunk . T.lines


multiLinedChunksByIndent    ::  T.Text
                            ->  [T.Text]
multiLinedChunksByIndent
    = multiLinedChunksBy$ \ _ -> T.null |.| (Data.Char.isSpace . T.head)
