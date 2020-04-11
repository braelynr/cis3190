!
!     Braelyn Rotman
!     1006740
!     Assignment 4
!     Due April 9 2020
!

      program multiplication
      implicit none
      integer :: m, n, p, multiplyRec, multiply
      real :: startT, endT, execTime

!     variable initialization
      m = 0
      n = 0
      p = 0

      write(*,*) "RUSSIAN PEASANT MULTIPLICATION"
      write(*,*) "Enter Positive Integer 1: "
      read(*,*) m
      write(*,*) "Enter Positive Integer 2: "
      read(*,*) n

!     calculate the product using the recursive function
      call cpu_time(startT)
      p = multiplyRec(m, n)
      call cpu_time(endT)
      execTime = (endT - startT)*1000


100   format(A16, A14, A15, A14, A20)
      write(*, 100) "Type |", "Multiplier |", "Multiplicand |", "Product |", "Execution Time (ms)"
      write(*,*) "Recursive ", "    |", m, "|", n, " |", p, "|", execTime

!     calculate the product using the non-recursive function
      call cpu_time(startT)
      p = multiply(m, n)
      call cpu_time(endT)
      execTime = (endT - startT)*1000

      write(*,*) "Non-Recursive ", "|", m, "|", n, " |", p, "|", execTime

      end program multiplication

!     THIS IS THE NON-RECURSIVE FUNCTION
      integer function multiply(m, n) result (p)
        implicit none
        integer, intent(in) :: m, n
        integer :: x, y
        x = m
        y = n
!       x is the Multiplier
!       y is the Multiplicand
!       p is the Product

        do while(x > 0)
          if (x == 1) then
            p = p + y
            return
          else if (x > 1 .and. (mod(x, 2) == 0)) then
            x = x / 2
            y = y + y
          else
            p = p + y
            x = x / 2
            y = y * 2
          end if
        end do
        return
      end function multiply

!     THIS IS THE RECURSIVE FUNCTION
      recursive function multiplyRec(m, n) result (p)
      implicit none
      integer, intent(in) :: m, n
      integer :: p
!     m is the Multiplier
!     n is the Multiplicand
!     p is the Product

      if (m == 0) then
        p = 0
        return
      else if (m == 1) then
        p = n
        return
      else if (m > 1 .and. (mod(m, 2) == 0)) then
        p = multiplyRec(m / 2, n + n)
        return
      else
        p = n + multiplyRec(m / 2, n * 2)
        return
      end if

      end function multiplyRec
