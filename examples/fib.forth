: fibonacci \ ( n -- )
    \ setup initial values for the sequence
    0 swap    \ put 0 (initial counter) on stack and move n to top
    0 1       \ put initial Fibonacci values (0, 1) on stack
    rot 0     \ put n on top and set loop start (0)

    do
        \ for each iteration, drop loop index since we don't need it
        drop

        \ print current Fibonacci number
        dup .  \ duplicate and print the current Fibonacci number
        cr     \ new line

        \ calculate next Fibonacci number
        swap   \ swap the two most recent Fibonacci numbers
        over   \ copy the second-to-top value to the top
        +      \ add to get the next Fibonacci number
    loop

    \ clean up the stack
    drop drop
;

\ example:
\ 10 fibonacci
