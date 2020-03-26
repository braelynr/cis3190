identification division.
program-id. sqrtbaby.
environment division.
input-output section.
file-control.
    select SYSIN assign to KEYBOARD
    organization is line sequential.
    select standard-output assign to display.
data division.
file section.
fd standard-output.
    01 out-line pic x(80).
working-storage section.
77 diff pic v9(5).
77 z    pic 9(11)v9(6).
77 k    pic S9999.
77 x    pic 9(11)v9(6).
77 y    pic 9(11)v9(6).
77 temp pic 9(11)v9(6).
01 eof  pic x(01) value "f".
01 in-card.
   02 in-z   pic s9(10)v9(6) sign leading separate.
01 print-line.
   02 filler pic x value space.
   02 out-z  pic z(11)9.9(6).
   02 filler pic x(5) value spaces.
   02 out-y  pic z(11)9.9(6).

procedure division.

S1.
    perform until eof = "t"
        display "Enter a number: " with no advancing
        accept in-z
        if eof = "f" then
            if in-z = 0 then
                perform finish
            end-if
            if in-z > 0 then
                perform B1
            end-if
            display "INVALID INPUT"
        end-if
    end-perform.
    perform finish.
end-S1.

B1.
    *>move in-diff to diff.
    move .00100 to diff.
    move in-z to z.
    compute x rounded = z / 2.
    PERFORM S2 THRU E2 VARYING K FROM 1 BY 1
        UNTIL K IS GREATER THAN 1000
    display "ATTEMPT ABORTED,TOO MANY ITERATIONS".
end-B1.

S2.
    compute y rounded = 0.5 * (X + Z / X).
    compute temp = y - x.
    if temp < 0 then
        compute temp = - temp
    end-if.
    if temp / (y + x) > diff then
        perform E2
    else
        move in-z to out-z
        move y to out-y
        display out-y
        perform S1
    end-if.
end-S2.

E2.
    move y to x.
end-E2.

FINISH.
    close sysin, standard-output.
    STOP RUN.
end-finish.
