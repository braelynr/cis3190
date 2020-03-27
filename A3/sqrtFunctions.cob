*> Assignment 3 - Cobol Re-engineering
*> March 27, 2020
*> Braelyn Rotman
*> 1006740

identification division.
program-id. sqrtFunctions.
environment division.

data division.
working-storage section.
77 diff       pic v9(5).
77 num        pic 9(11)v9(6).
77 k          pic s9999.
77 previous   pic 9(11)v9(6).
77 current    pic 9(11)v9(6).
77 temp       pic s9(11)v9(6).

*> diff is the accuracy required. It is set to 0.001
*> num is the number to find the square root of
*> k is for iterations
*> previous is the previous estimate R(k-1)
*> next is the current estimate R(k)
*> temp is used to calculate the accuracy


linkage section.
01 userInput   pic s9(10)v9(6) sign leading separate.
01 result     pic z(11)9.9(6).

*> userInput is the number retreived from the main program
*> result is the calculated root

procedure division using userInput, result.

squareroot.
  *> initializing variables
  move 0 to result.
  move .00100 to diff.
  move 1 to k.
  move userInput to num.

  *> The first previous, R(0) could be any random number. The program uses num/2
  compute previous rounded = num / 2.

  perform until k > 1000
    compute current rounded = (previous + num / previous) / 2
    compute temp = current - previous
    if temp < 0 then
        compute temp = - temp
    end-if

    *> If the accuracy has not been reached, set the current to the previous and iterate again
    if temp / (current + previous) > diff then
        move current to previous
    else
        *> The accuracy has been reached and the result can be sent back to the main
        move current to result
        exit program
    end-if
    compute k = k + 1
  end-perform.

  move 0 to result.
  display "ATTEMPT ABORTED,TOO MANY ITERATIONS".
  display " ".
end-squareroot.

exit program.
