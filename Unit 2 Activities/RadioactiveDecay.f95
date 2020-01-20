      program RadioactiveDecay
!
!     A radioactive isotope has a half-life of 16 days.
!     You wish to have 30g at the end of 30 days.
!     This program determines how much of the radioisotope
!     you should start with.
!
      integer :: T, N, time
      real :: N0, lambda
      T = 16
      N = 30
      time = 30
      lambda = log(2.0) / T
      N0 = N * exp(lambda*time)
      write (*,20) 'You should start with', N0, 'grams.'
20    format(a,1f10.5,a)
      end program RadioactiveDecay
