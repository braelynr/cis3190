      program SunKink
!
!     Calculates the length of a rail track
!     dependent on the temperature
!
      real :: Linit, Lnew, Tinit, Tnew, alpha
      Linit = 300.0
      Tinit = 15.0
      Tnew = 60.0
      alpha = +11.0E-6
      Lnew = alpha * Linit * (Tnew-Tinit)
      write(*,20) 'The change in length of the rail is ', Lnew, 'm'
20    format(a,1f6.4,a)
      end program SunKink
