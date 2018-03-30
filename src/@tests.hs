module Main where

import qualified Dbg
import qualified Str

main    :: IO ()
main =
    readFile "d:\\c" >>= \ src
    -> putStr$ Dbg.autoIndent$ show$ Str.multiLinedChunksByIndent src
