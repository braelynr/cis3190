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
01 out-y  pic z(11)9.9(6).

linkage section.
01 in-z   pic s9(10)v9(6) sign leading separate.

procedure division using in-z.

squareroot.
  move .00100 to diff.
  move 1 to k.
  move in-z to z.
  compute x rounded = z / 2.

  perform until k > 1000
    compute y rounded = 0.5 * (x + z / x)
    compute temp = y - x
    if temp < 0 then
        compute temp = - temp
    end-if
    if temp / (y + x) > diff then
        move y to x
    else
        move y to out-y
        display "k = " k
        display "Square Root = " out-y
        display " "
        exit program
    end-if
    compute k = k + 1
  end-perform.

  display "ATTEMPT ABORTED,TOO MANY ITERATIONS".
  display " ".
end-squareroot.


exit program.
