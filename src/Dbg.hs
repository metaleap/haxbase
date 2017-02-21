module Dbg where

import qualified Data.List



autoIndent  ::  String
            -> String
autoIndent
    = ind (0::Int , False , False , False)
    where
    ind _ "" = ""
    ind (ind1 , instr1 , inchr1 , inesc1) (c:rest) =
        c : nuln ++ ind (ind2 , instr2 , inchr2 , inesc2) rest
        where
        inany = instr2 || inchr2 || inesc2
        inesc2 = (instr2 || inchr2) && (not inesc1) && (c=='\\')
        instr2  | (instr1) = c/='\"' || inesc1
                | (otherwise) = c=='\"' && not (inchr1 || inesc1)
        inchr2  | (inchr1) = c/='\'' || inesc1
                | (otherwise) = c=='\'' && not (instr1 || inesc1)
        ind2    | (inany) = ind1
                | (otherwise) = i c where
                    i '{' = ind1 + 1 ; i '}' = ind1 - 1 ; i _ = ind1
        nuln    | (inany || not (n c)) = ""
                | (otherwise) = '\n' : Data.List.replicate ind2 '\t'
                where n '{' = True ; n ',' = True ; n _ = False
