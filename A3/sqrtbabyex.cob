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

01 eof  pic x(01) value "f".
01 in-z   pic s9(10)v9(6) sign leading separate.

procedure division.

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
                call "sqrtFunctions" using in-z
            else
              display "INVALID INPUT"
              display " "
            end-if
        end-if
    end-perform.
    perform finish.
end-S1.

FINISH.
    close sysin, standard-output.
    stop run.
end-FINISH.
