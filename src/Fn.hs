module Fn
where



times ::  Int  ->  (b->b)  ->  b  ->  b
times 0 _ =
    id
times n func =
    (times (n - 1) func) . func
