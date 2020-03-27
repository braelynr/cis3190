*> Assignment 3 - Cobol Re-engineering
*> March 27, 2020
*> Braelyn Rotman
*> 1006740

identification division.
program-id. sqrtbaby.

environment division.
input-output section.
file-control.
    select sysin assign to keyboard
    organization is line sequential.
    select standard-output assign to display.

data division.
file section.
fd standard-output.
    01 out-line pic x(80).
working-storage section.
77 diff pic v9(5).
77 z    pic 9(11)v9(6).
77 k    pic s9999.
77 x    pic 9(11)v9(6).
77 y    pic 9(11)v9(6).
77 temp pic s9(11)v9(6).

*> diff is the accuracy required. It is set to 0.001
*> z is the number to find the square root of
*> k is for iterations
*> x is the previous estimate R(k-1)
*> y is the current estimate R(k)
*> temp is used to calculate the accuracy

01 eof  pic x(01) value "f".
01 in-z   pic s9(10)v9(6) sign leading separate.
01 out-y  pic z(11)9.9(6).

*> eof is used to determine when the user is done inputting numbers (originally the end of the input file)
*> in-z is the user input
*> out-y is the resulting square root

procedure division.

*> S1 requests user input until the number 0 is entered (originally until the end of the file is reached)
S1.
    perform until eof = "t"
        display "Enter a number: " with no advancing
        accept in-z
        if eof = "f" then
            if in-z = 0 then
                move "t" to eof
                perform finish
            end-if
            if in-z > 0 then
                perform B1
            end-if
            *> This will display if a negative number is entered
            display "INVALID INPUT"
            display " "
        end-if
    end-perform.
    perform finish.
end-S1.

*> B1 runs through the algorithm until the accuracy is reached or 1000 iterations are completed
B1.
    move .00100 to diff.
    move in-z to z.
    compute x rounded = z / 2.
    perform S2 thru E2 varying k from 1 by 1
        until k > 1000
    display "ATTEMPT ABORTED,TOO MANY ITERATIONS".
    display " ".
end-B1.

*> S1 calculates each estimate
S2.
    compute y rounded = 0.5 * (x + z / x).
    compute temp = y - x.
    if temp < 0 then
        compute temp = - temp
    end-if.

    *> if the accuracy is not reached the algorithm will continue
    if temp / (y + x) > diff then
        perform E2
    else
        *> The accuracy has been reached
        move y to out-y

        display "Square Root = " out-y
        display " "
        perform S1
    end-if.
end-S2.

*> E2 sets the current estimate to the previous estimate for the next iteration
E2.
    move y to x.
end-E2.

finish.
    close sysin, standard-output.
    stop run.
end-finish.
