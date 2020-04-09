-- Braelyn Rotman
-- CIS*3190
-- Assignment 4
-- Due April 9 2020

with ada.Text_IO; use Ada.Text_IO;
with ada.Long_Integer_Text_IO; use Ada.Long_Integer_text_IO;
with Ada.Calendar; use Ada.Calendar;
procedure multiplication is
  m, n, p : Long_Integer := 0;

  type functype is access function (x,y: Long_Integer) return Long_Integer;

-- This function is from craftofcoding.wordpress.com
  procedure timeit(Func : functype; m,n : in Long_Integer) is
   result : Long_Integer;
   startTime, endTime : time;
   milliS : Duration;
  begin
     startTime := Clock;
     result := Func(m,n);
     endTime := Clock;
     milliS := (endTime - startTime) * 1000;
     put_line(Long_Integer'image(m) & " x" & Long_Integer'image(n) & " =" & Long_Integer'image(result));
     put_line("Runtime = " & Duration'Image(milliS) & " milliseconds.");
  end timeit;

-- Recursive Function
  function multiplyRec (m, n : Long_Integer) return Long_Integer is
  -- m is the Multiplier
  -- n is the Multiplicand

  begin
    if (m = 0) then
      return 0;
    elsif (m = 1) then
      return n;
    elsif (m > 1 and (m mod 2 = 0)) then
      return multiplyRec(m / 2, n + n);
    else
      return n + multiplyRec(m / 2, n * 2);
    end if;

  end multiplyRec;

--  Non-Recursive Function
function multiply (m, n : Long_Integer) return Long_Integer is
  x, y : Long_Integer;
begin
  x := m;
  y := n;
  -- x is the Multiplier
  -- y is the Multiplicand
  -- p is the Product

  loop
    exit when x = 0;
    if (x = 1) then
      p := p + y;
      return p;
    elsif (x > 1 and (x mod 2 = 0)) then
      x := x / 2;
      y := y + y;
    else
      p := p + y;
      x := x / 2;
      y := y * 2;
    end if;
  end loop;
  return 0;

end multiply;


begin

  put("Enter Integer 1: ");
  get(m);
  put("Enter Integer 2: ");
  get(n);

  new_line;
  put_line("RUSSIAN PEASANT MULTIPLICATION");
  put_line("Recursive Function:");
  -- Call recursive function
  timeit(multiplyRec'Access, m, n);

  new_line;
  put_line("Non-Recursive Function:");
  -- Call non-recursive function
  timeit(multiply'Access, m, n);

end multiplication;
