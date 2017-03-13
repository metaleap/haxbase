module Base
(module Base , (Control.Applicative.<|>) , (Data.Monoid.<>) , (System.FilePath.</>))
where

import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Foldable
import Data.Function ( (&) )
import qualified Data.Maybe
import qualified Data.Monoid
import qualified Data.Traversable
import qualified System.FilePath



duo ::  (i1->o1 , i2->o2)  ->  (i1 , i2)
    ->  (o1 , o2)
duo (func1 , func2) (tup1 , tup2)
    = (func1 tup1 , func2 tup2)


dupl    ::  a
        ->  (a , a)
dupl    x
    = (x , x)


err ::  String
    ->  a
err
    = errorWithoutStackTrace



has ::  [a]
    ->  Bool
has [] = False
has (_:_) = True




(~.)    ::  (a->b)  ->  (b->c)  ->  a
        ->  c
infixl  ~.
(~.)    = flip (.)


(|.|)   ::  (a->Bool)  ->  (a->Bool)
        ->  (a->Bool)
infix   8 |.|
(|.|) check1 check2 val
    = check1 val || check2 val


(&.&)   ::  (a->Bool)  ->  (a->Bool)
        ->  (a->Bool)
infix   8 &.&
(&.&) check1 check2 val
    = check1 val && check2 val




(-:)    ::  a  ->  (a->b)
        ->  b
infixl  9 -:
(-:)    = (&)


(~>)    ::  a  ->  (a->b)
        ->  b
infixl  ~>
(~>)    = (&)




(=:)    ::  a  ->  b
        ->  (a,b)
infix   0 =:
(=:)    = (,)




(|~)    ::  (a->Bool)  ->  [a]
        ->  [a]
infixr  7 |~
(|~)    = filter


(~|)    ::  [a]  ->  (a->Bool)
        ->  [a]
infixl  7 ~|
(~|)    = flip filter




(>~)    ::  (Functor f)
        =>  f a  ->  (a->b)
        ->  f b
infixl  8 >~
(>~)    = flip fmap


(>=~)   ::  [a]  ->  (a->Maybe b)
        ->  [b]
infixl  9 >=~
(>=~)   = flip Data.Maybe.mapMaybe


(>/~)   ::  [a]  ->  (a->[b])
        ->  [[b]]
infixl  9 >/~
(>/~)   [] _
    = []
(>/~)   (item:more) func
    = discardnil (func item) where
        discardnil []   = more >/~ func
        discardnil val  = val : (more >/~ func)


(>>~)   ::  (Traversable t, Applicative m)
        =>  t a  ->  (a->m b)
        ->  m (t b)
infixl  8 >>~
(>>~)   = Data.Traversable.for


(*>~)   ::  (Applicative m, Foldable f)
        =>  f a  ->  (a->m b)
        ->  m ()
infixl  8 *>~
(*>~)   = Data.Foldable.for_



(>>|)   ::  (Applicative m)
        =>  [a]  ->  (a->m Bool)
        ->  m [a]
infixl  >>|
(>>|)   = flip Control.Monad.filterM




(|?)    ::  Bool  ->  a  ->  a
        ->  a
infix   1 |?
(|?)    True yay _ = yay
(|?)    False _ nay = nay


(|!)    ::  (a->b)  ->  a
        ->  b
infixr  0 |!
(|!)    = ($)



(<?>)   ::  [a]  ->  [a]
        ->  [a]
infix   9 <?>
[] <?> list = list
list <?> _ = list



(-|=)   ::  a  ->  Maybe a
        ->  a
infix   1 -|=
(-|=)   = Data.Maybe.fromMaybe


(=|-)   ::  (a->b)  ->  b  ->  Maybe a
        ->  b
infix   0 =|-
(=|-)   = flip maybe




(@!)    ::  [a]  -> Int
        ->  a
infixl  9 @!
[] @! _ = undefined  --  rids this Careful Coder (TM) of the pesky 'non-exhaustive patterns' warning
(x:_) @! 0              = x
(_:x:_) @! 1            = x
-- (_:_:x:_) @! 2       = x
-- (_:_:_:x:_) @! 3     = x
-- (_:_:_:_:x:_) @! 4   = x
list @! i               = (drop i list) @! 0



(@?)    ::  [a]  ->  Int
        ->  Maybe a
infixl  9 @?
[] @? _                 = Nothing
(x:_) @? 0              = Just x
(_:x:_) @? 1            = Just x
-- (_:_:x:_) @? 2       = Just x
-- (_:_:_:x:_) @? 3     = Just x
-- (_:_:_:_:x:_) @? 4   = Just x
list @? i               = (drop i list) @? 0
