identification division.
program-id. sqrtFunctions.
environment division.

data division.
working-storage section.
77 diff pic v9(5).
77 z    pic 9(11)v9(6).
77 k    pic s9999.
77 x    pic 9(11)v9(6).
77 y    pic 9(11)v9(6).
77 temp pic s9(11)v9(6).
01 print-line.
   02 filler pic x value space.
   02 out-z  pic z(11)9.9(6).
   02 filler pic x(5) value spaces.
   02 out-y  pic z(11)9.9(6).

linkage section.
01 in-z   pic s9(10)v9(6) sign leading separate.

procedure division using in-z.

B1.
    move .00100 to diff.
    move in-z to z.
    compute x rounded = z / 2.
    perform S2 thru E2 varying k from 1 by 1
        until k > 1000
    display "ATTEMPT ABORTED,TOO MANY ITERATIONS".
    display " ".
end-B1.

S2.
    compute y rounded = 0.5 * (x + z / x).
    compute temp = y - x.
    if temp < 0 then
        compute temp = - temp
    end-if.
    if temp / (y + x) > diff then
        perform E2
    else
        move in-z to out-z
        move y to out-y

        display "Square Root = " out-y
        display " "
        exit program
    end-if.
end-S2.

E2.
    move y to x.
end-E2.

exit program.
