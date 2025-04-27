\ count from 1 to a given number

: countto
    0
    begin
        1 +
        dup .

        over over =
        if
            drop drop
            exit
        then
    again
;
