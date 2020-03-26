identification division.
program-id. sqrtbaby.
environment division.
input-output section.
file-control.
    *> select input-file assign to "sqrtFIXED.dat"
    select SYSIN assign to KEYBOARD
    organization is line sequential.
    select standard-output assign to display.
data division.
file section.
    *> fd input-file.
    *>    01 standard-input pic x(80).
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
   *> 02 in-z     pic s9(10)v9(6) sign leading separate.
   02 in-z   pic s9(10)v9(6) sign leading separate.
   *> 02 in-diff  pic v9(5).
   *> 02 filler   pic x(58).
01 title-line.
   02 filler pic x(9) value spaces.
   02 filler pic x(26) value 'SQUARE ROOT APPROXIMATIONS'.
01 under-line.
   02 filler pic x(44) value
      '--------------------------------------------'.
01 col-heads.
   02 filler pic x(8) value spaces.
   02 filler pic x(6) value 'NUMBER'.
   02 filler pic x(15) value spaces.
   02 filler pic x(11) value 'SQUARE ROOT'.
01 underline-2.
   02 filler pic x(20) value ' -------------------'.
   02 filler pic x(5) value spaces.
   02 filler pic x(19) value '------------------'.
01 print-line.
   02 filler pic x value space.
   02 out-z  pic z(11)9.9(6).
   02 filler pic x(5) value spaces.
   02 out-y  pic z(11)9.9(6).
01 error-mess.
   02 filler pic x value space.
   02 ot-z   pic -(11)9.9(6).
   02 filler pic x(21) value '        INVALID INPUT'.
01 abort-mess.
   02 filler pic x value space.
   02 outp-z pic z(11)9.9(6).
   02 filler pic x(37) value
      '  ATTEMPT ABORTED,TOO MANY ITERATIONS'.

procedure division.
    *> open input input-file, output standard-output.open
    open input sysin, output standard-output.open
    write out-line from title-line after advancing 0 lines.
    write out-line from under-line after advancing 1 line.
    write out-line from col-heads after advancing 1 line.
    write out-line from underline-2 after advancing 1 line.

S1.
    perform until eof = "t"
        read sysin into in-z
        at end
            move "t" to eof
        end-read
        display in-card
        if eof = "f" then
            if in-z = 0 then
                perform finish
            end-if
            if in-z > 0 then
                perform B1
            end-if
            move in-z to ot-z
            write out-line from error-mess after advancing 1 line
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
    move in-z to outp-z.
    write out-line from abort-mess after advancing 1 line.
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
        write out-line from print-line after advancing 1 line
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
