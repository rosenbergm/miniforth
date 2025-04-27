\ GCD using Euclidean algorithm (iterative)


: gcd \ ( a b -- )
    begin
        \ if b = 0, we're done
        dup 0 = if
            drop    \ remove b, leaving just a (the result)
            .
            exit
        then

        \ a mod b
        swap over mod
    again
;
