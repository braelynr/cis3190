      program WaveBreaking
!
!     Determines if a wave is breaking and if it is
!     spilling, plunging, or surging. A breakng wave is one whose
!     base can no longer support its top, causing it to collapse.
!
      real :: T, H, m, g, B
      g = 981.0
      write(*,*) 'Enter the wave period (seconds): '
      read(*,*) T
      write(*,*) 'Enter the wave height (cm): '
      read(*,*) H
      write(*,*) 'Enter the beach slope: '
      read(*,*) m
      B = H / (g*m*T*T)
      if(B < 0.003) then
          write(*,*) 'The breakers are surging.'
      else if (B > 0.068) then
          write(*,*) 'The breakers are spilling.'
      else
          write(*,*) 'The breakers are plunging.'
      end if
      end program WaveBreaking
