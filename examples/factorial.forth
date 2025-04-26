: factorial
    \ handle 0! = 1
    dup 0 = if
        drop 1
        exit
    then

    \ initialize product to 1
    1           \ stack: n 1
    swap        \ stack: 1 n
    1+          \ stack: 1 n+1 (for loop bounds)
    1           \ stack: 1 n+1 1

    do
      *     \ multiply accumulator by loop index
    loop        \ stack: result

    .
;
