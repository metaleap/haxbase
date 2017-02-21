module Fn
where



repeatedly  ::  (Eq a)
            =>  (a->a)  ->  a
            ->  a
repeatedly  fn arg
    = if (result==arg) then result else repeatedly fn result
    where result = fn arg



times   ::  Int  ->  (b->b)  ->  b
        ->  b
times   0 _
    = id
times   n func
    = (times (n - 1) func) . func
