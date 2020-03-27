*> Assignment 3 - Cobol Re-engineering
*> March 27, 2020
*> Braelyn Rotman
*> 1006740

identification division.
program-id. sqrtbabyex.

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
01 eof        pic x(01) value "f".
01 userInput  pic s9(10)v9(6) sign leading separate.
01 result     pic z(11)9.9(6).

*> eof is a flag for when the user enters 0 (to exit the program), originally end of the input file
*> userInput is the number to find the square root of

procedure division.

*> S1 is the main function to receive user input and call the external function to calculate the square root
S1.
    perform until eof = "t"
        display "Enter a number: " with no advancing
        accept userInput
        if eof = "f" then
            if userInput = 0 then
                move "t" to eof
                perform finish
            end-if
            if userInput > 0 then
                call "sqrtFunctions" using userInput, result
                *> if result is 0 then an error occured (too many iterations were executed in the subprogram)
                if result not = 0 then
                  display "Square Root = " result
                  display " "
                end-if
            else
              display "INVALID INPUT"
              display " "
            end-if
        end-if
    end-perform.
    perform finish.
end-S1.

finish.
    close sysin, standard-output.
    stop run.
end-finish.
