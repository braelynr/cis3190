      module hex
          implicit none
      contains
!         Subroutine to obtain input for a word to be encrypted by the user
          subroutine readWord(w)
          character(len=11), intent(out) :: w

          do while(len_trim(w) == 11)
              write(*,*) (' Enter word:')
              read(*,*) w
              if(len_trim(w) == 11) write(*,*) ('Invalid Word Length')
          end do
          end subroutine readWord

!         Subroutine to convert the user input into hexadecimal
          subroutine word2hex(w, h, l)

          character (len = 11), intent(in) :: w
          integer, intent(out), dimension(0:31) :: h
          integer, intent(out) :: l
          character (len=2) :: tempH
          integer, dimension(1:2) :: hex

          integer :: i, j

          j = 0
          do i = 1, len_trim(w)
              write(tempH, '(Z2)') w(i:i)
              read(tempH(1:1),'(Z1)')hex(1)
              read(tempH(2:2),'(Z1)')hex(2)
              h(j) = hex(1)
              h(j+1) = hex(2)
              j = j + 2
              l = l + 2
          end do

          end subroutine word2hex

!         Subroutine to print the hex version of the word
          subroutine printhex(h, l)
          integer, intent(in) :: l
          integer, intent(in), dimension(0:l) :: h
          integer :: i

          write (*,1100)
          write(*,1101)(h(i), i=0, l-1)

1100      format(' Hexadecimal word: '16(1x,i1))
1101      format(32z1.1)
          end subroutine printhex
      end module hex
