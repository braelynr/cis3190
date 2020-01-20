      program average
!
!     This program reads in three numbers and
!     sums and averages them.
!
      real :: num1, num2, num3, avg, total
      integer :: number
      number = 3
      total = 0.0
      write (*,*) 'Enter three numbers: '
      read (*,*) num1, num2, num3
      total = num1 + num2 + num3
      avg = total / number
      write (*,*) 'The sum of the numbers is ', total
      write (*,*) 'Thge average of the numbers is ', avg
      end
